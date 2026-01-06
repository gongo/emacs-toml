;;; toml-test-official.el --- Tests using official toml-test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>

;;; Commentary:

;; This file contains tests that use the official toml-test suite
;; from https://github.com/toml-lang/toml-test
;;
;; The test suite is included as a git submodule at tests/toml-test/
;; Test file list is read from tests/toml-test/tests/files-toml-1.1.0

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'toml)
(require 'json)

(defvar toml-test-official:base-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Base directory of the toml package.")

(defvar toml-test-official:test-dir
  (expand-file-name "tests/toml-test/tests/" toml-test-official:base-dir)
  "Directory containing the official toml-test files.")

(defvar toml-test-official:file-list
  (expand-file-name "files-toml-1.1.0" toml-test-official:test-dir)
  "File containing the list of test files for TOML 1.1.0.")

;;; Helper functions for converting parsed TOML to toml-test JSON format

(defun toml-test-official:value-to-tagged (value)
  "Convert a parsed TOML VALUE to the tagged format used by toml-test.
The toml-test format uses {\"type\": TYPE, \"value\": VALUE} for scalars."
  (cond
   ;; nil/false -> boolean false
   ((eq value nil)
    '(("type" . "bool") ("value" . "false")))
   ;; t -> boolean true
   ((eq value t)
    '(("type" . "bool") ("value" . "true")))
   ;; Vector -> array
   ((vectorp value)
    (mapcar #'toml-test-official:value-to-tagged (append value nil)))
   ;; Alist (table) -> recurse
   ((and (listp value) (consp (car value)))
    (toml-test-official:alist-to-tagged value))
   ;; Integer
   ((integerp value)
    `(("type" . "integer") ("value" . ,(number-to-string value))))
   ;; Float
   ((floatp value)
    `(("type" . "float") ("value" . ,(number-to-string value))))
   ;; String
   ((stringp value)
    `(("type" . "string") ("value" . ,value)))
   ;; Datetime list (seconds minutes hour day month year)
   ((and (listp value) (= (length value) 6) (numberp (car value)))
    (let ((seconds (nth 0 value))
          (minutes (nth 1 value))
          (hour (nth 2 value))
          (day (nth 3 value))
          (month (nth 4 value))
          (year (nth 5 value)))
      `(("type" . "datetime")
        ("value" . ,(format "%04d-%02d-%02dT%02d:%02d:%02dZ"
                            year month day hour minutes seconds)))))
   ;; Fallback
   (t value)))

(defun toml-test-official:alist-to-tagged (alist)
  "Convert an ALIST (parsed TOML table) to the tagged format."
  (let (result)
    (dolist (pair alist)
      (let ((key (car pair))
            (value (cdr pair)))
        (push (cons key (toml-test-official:value-to-tagged value)) result)))
    (nreverse result)))

(defun toml-test-official:parse-json-file (file)
  "Parse JSON FILE and return as Lisp structure."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'string))
      (json-read))))

(defun toml-test-official:compare-values (expected actual path)
  "Compare EXPECTED and ACTUAL values, reporting differences at PATH."
  (cond
   ;; Both are alists (tables)
   ((and (listp expected) (listp actual)
         (or (null expected) (consp (car expected)))
         (or (null actual) (consp (car actual))))
    (let ((all-keys (delete-dups (append (mapcar #'car expected)
                                          (mapcar #'car actual)))))
      (cl-block nil
        (dolist (key all-keys)
          (let ((exp-val (cdr (assoc key expected)))
                (act-val (cdr (assoc key actual))))
            (unless (toml-test-official:compare-values
                     exp-val act-val (concat path "." key))
              (cl-return nil))))
        t)))
   ;; Both are lists (arrays)
   ((and (listp expected) (listp actual)
         (not (and expected (consp (car expected)))))
    (if (= (length expected) (length actual))
        (let ((idx 0) (result t))
          (while (and result (< idx (length expected)))
            (unless (toml-test-official:compare-values
                     (nth idx expected) (nth idx actual)
                     (format "%s[%d]" path idx))
              (setq result nil))
            (setq idx (1+ idx)))
          result)
      (message "Array length mismatch at %s: expected %d, got %d"
               path (length expected) (length actual))
      nil))
   ;; Scalar comparison
   (t
    (if (equal expected actual)
        t
      (message "Value mismatch at %s: expected %S, got %S"
               path expected actual)
      nil))))

;;; Test runner for valid TOML files

(defun toml-test-official:run-valid-test (toml-file json-file)
  "Run a valid test: parse TOML-FILE and compare with JSON-FILE."
  (condition-case err
      (let* ((parsed (toml:read-from-file toml-file))
             (tagged (toml-test-official:alist-to-tagged parsed))
             (expected (toml-test-official:parse-json-file json-file)))
        (toml-test-official:compare-values expected tagged ""))
    (error
     (message "Error parsing %s: %S" toml-file err)
     nil)))

(defun toml-test-official:run-invalid-test (toml-file)
  "Run an invalid test: ensure parsing TOML-FILE signals an error."
  (condition-case _err
      (progn
        (toml:read-from-file toml-file)
        ;; If we get here, parsing succeeded when it should have failed
        (message "Expected error but parsing succeeded for %s" toml-file)
        nil)
    (toml-error t)
    (error
     ;; Other errors are also acceptable for invalid TOML
     t)))

;;; Read test file list and generate ERT tests

(defun toml-test-official:read-file-list ()
  "Read the test file list from files-toml-1.1.0.
Returns a plist with :valid and :invalid lists of file paths."
  (let (valid-files invalid-files)
    (when (file-exists-p toml-test-official:file-list)
      (with-temp-buffer
        (insert-file-contents toml-test-official:file-list)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match "\\.toml\\'" line)
              (cond
               ((string-prefix-p "valid/" line)
                (push line valid-files))
               ((string-prefix-p "invalid/" line)
                (push line invalid-files)))))
          (forward-line 1))))
    (list :valid (nreverse valid-files)
          :invalid (nreverse invalid-files))))

(defun toml-test-official:make-test-name (file-path)
  "Convert FILE-PATH to a test name symbol.
e.g., \"valid/array/array.toml\" -> `toml-test-official:valid/array/array'"
  (let ((name (file-name-sans-extension file-path)))
    (intern (format "toml-test-official:%s" name))))

(defun toml-test-official:generate-tests ()
  "Generate ERT tests from the file list."
  (let* ((file-list (toml-test-official:read-file-list))
         (valid-files (plist-get file-list :valid))
         (invalid-files (plist-get file-list :invalid)))
    ;; Generate valid tests
    (dolist (toml-path valid-files)
      (let* ((full-toml-path (expand-file-name toml-path toml-test-official:test-dir))
             (full-json-path (concat (file-name-sans-extension full-toml-path) ".json"))
             (test-name (toml-test-official:make-test-name toml-path)))
        (when (and (file-exists-p full-toml-path)
                   (file-exists-p full-json-path))
          (eval `(ert-deftest ,test-name ()
                   ,(format "Test valid TOML: %s" toml-path)
                   (should (toml-test-official:run-valid-test
                            ,full-toml-path ,full-json-path)))))))
    ;; Generate invalid tests
    (dolist (toml-path invalid-files)
      (let* ((full-toml-path (expand-file-name toml-path toml-test-official:test-dir))
             (test-name (toml-test-official:make-test-name toml-path)))
        (when (file-exists-p full-toml-path)
          (eval `(ert-deftest ,test-name ()
                   ,(format "Test invalid TOML: %s" toml-path)
                   (should (toml-test-official:run-invalid-test
                            ,full-toml-path)))))))))

;; Generate tests when this file is loaded
(toml-test-official:generate-tests)

(provide 'toml-test-official)

;;; toml-test-official.el ends here
