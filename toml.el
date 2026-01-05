;;; toml.el --- TOML (Tom's Obvious, Minimal Language) parser -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-toml
;; Keywords: toml parser
;; Version: 0.0.1

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This is a library for parsing TOML (Tom's Obvious, Minimal
;; Language).

;; Learn all about TOML here: https://github.com/mojombo/toml

;; Inspired by json.el.  thanks!!

;;; Code:

(require 'parse-time)

(defconst toml->special-escape-characters
  '(?b ?t ?n ?f ?r ?\" ?\/ ?\\)
  "Characters which are escaped in TOML.

\\b     - backspace       (U+0008)
\\t     - tab             (U+0009)
\\n     - linefeed        (U+000A)
\\f     - form feed       (U+000C)
\\r     - carriage return (U+000D)
\\\"     - quote           (U+0022)
\\\/     - slash           (U+002F)
\\\\     - backslash       (U+005C)

notes:

 Excluded four hex (\\uXXXX).  Do check in `toml:read-escaped-char'")

(defconst toml->parse-dispatch-table
  (let ((table
         '((?t  . toml:read-boolean)
           (?f  . toml:read-boolean)
           (?\[ . toml:read-array)
           (?{  . toml:read-inline-table)
           (?\" . toml:read-string)
           (?\' . toml:read-literal-string))))
    (mapc (lambda (char)
            (push (cons char 'toml:read-start-with-number) table))
          '(?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table))

(defconst toml->regexp-datetime
  "\
\\([0-9]\\{4\\}\\)-\
\\(0[1-9]\\|1[0-2]\\)-\
\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)T\
\\([0-1][0-9]\\|2[0-4]\\):\
\\([0-5][0-9]\\):\
\\([0-5][0-9]\\)Z"
  "Regular expression for a datetime (Zulu time format).")

(defconst toml->regexp-numeric
  "\\(-?[0-9]+[\\.0-9\\]*\\)"
  "Regular expression for a numeric.")

;; Error conditions

(put 'toml-error 'error-message "Unknown TOML error")
(put 'toml-error 'error-conditions '(toml-error error))

(put 'toml-string-error 'error-message "Bad string")
(put 'toml-string-error 'error-conditions
     '(toml-string-error toml-error error))

(put 'toml-string-escape-error 'error-message "Bad escaped string")
(put 'toml-string-escape-error 'error-conditions
     '(toml-string-escape-error toml-string-error toml-error error))

(put 'toml-string-unicode-escape-error 'error-message "Bad unicode escaped string")
(put 'toml-string-unicode-escape-error 'error-conditions
     '(toml-string-unicode-escape-error
       toml-string-escape-error
       toml-string-error toml-error error))

(put 'toml-boolean-error 'error-message "Bad boolean")
(put 'toml-boolean-error 'error-conditions
     '(toml-boolean-error toml-error error))

(put 'toml-datetime-error 'error-message "Bad datetime")
(put 'toml-datetime-error 'error-conditions
     '(toml-datetime-error toml-error error))

(put 'toml-numeric-error 'error-message "Bad numeric")
(put 'toml-numeric-error 'error-conditions
     '(toml-numeric-error toml-error error))

(put 'toml-start-with-number-error 'error-message "Bad start-with-number")
(put 'toml-start-with-number-error 'error-conditions
     '(toml-start-with-number-error toml-error error))

(put 'toml-array-error 'error-message "Bad array")
(put 'toml-array-error 'error-conditions
     '(toml-array-error toml-error error))

(put 'toml-key-error 'error-message "Bad key")
(put 'toml-key-error 'error-conditions
     '(toml-key-error toml-error error))

(put 'toml-table-error 'error-message "Bad table")
(put 'toml-table-error 'error-conditions
     '(toml-table-error toml-error error))

(put 'toml-value-error 'error-message "Bad readable value")
(put 'toml-value-error 'error-conditions
     '(toml-value-error toml-error error))

(put 'toml-redefine-table-error 'error-message "Redefine table error")
(put 'toml-redefine-table-error 'error-conditions
     '(toml-redefine-table-error toml-error error))

(put 'toml-redefine-key-error 'error-message "Redefine key error")
(put 'toml-redefine-key-error 'error-conditions
     '(toml-redefine-key-error toml-error error))

(put 'toml-array-table-error 'error-message "Bad array of tables")
(put 'toml-array-table-error 'error-conditions
     '(toml-array-table-error toml-error error))

(defun toml:assoc (keys hash)
  "Look up nested KEYS in HASH and return the found element.

Example:
  (toml:assoc \\='(\"servers\" \"alpha\" \"ip\") hash)"
  (let (element)
    (catch 'break
      (dolist (k keys)
        (unless (toml:alistp hash) (throw 'break nil))
        (setq element (assoc k hash))
        (if element
            (setq hash (cdr element))
          (throw 'break nil)))
      element)))

(defun toml:alistp (alist)
  "Return t if ALIST is a list of association lists, nil otherwise."
  (when (listp alist)
    (catch 'break
      (dolist (al alist)
        (unless (consp al) (throw 'break nil)))
      t)))

(defun toml:end-of-line-p ()
  (looking-at "$"))

(defun toml:get-char-at-point ()
  (char-after (point)))

(defun toml:seek-beginning-of-next-line ()
  "Move point to beginning of next line."
  (forward-line)
  (beginning-of-line))

(defun toml:seek-readable-point ()
  "Move point forward, stopping readable point. (toml->parse-dispatch-table).

Skip target:

- whitespace (Tab or Space)
- comment line (start with hash symbol)"
  (toml:seek-non-whitespace)
  (while (and (not (eobp))
              (char-equal (toml:get-char-at-point) ?#))
    (end-of-line)
    (unless (eobp)
      (toml:seek-beginning-of-next-line)
      (toml:seek-non-whitespace))))

(defun toml:seek-non-whitespace ()
  "Move point forward, stopping before non-whitespace char or EOB."
  (if (re-search-forward "[^ \t\n]" nil t)
      (backward-char)
    (re-search-forward "[ \t\n]+\\'" nil t)))

(defun toml:search-forward (regexp)
  "Search forward from point for regular expression REGEXP.
Move point to the end of the occurrence found, and return point."
  (when (looking-at regexp)
    (forward-char (length (match-string-no-properties 0)))
    t))

(defun toml:read-char (&optional char-p)
  "Read character at point.  Set point to next point.
If CHAR-P is nil, return character as string,
and not nil, return character as char.

Move point a character forward."
  (let ((char (toml:get-char-at-point)))
    (forward-char)
    (if char-p char
      (char-to-string char))))

(defun toml:read-escaped-char ()
  "Read escaped character at point.  Return character as string.
Move point to the end of read characters."
  (unless (eq ?\\ (toml:read-char t))
    (signal 'toml-string-escape-error (list (point))))
  (let* ((char (toml:read-char t))
         (special (memq char toml->special-escape-characters)))
    (cond
     (special (concat (list ?\\ char)))
     ((and (eq char ?u)
           (toml:search-forward "[0-9A-Fa-f]\\{4\\}"))
      (concat "\\u" (match-string 0)))
     (t (signal 'toml-string-unicode-escape-error (list (point)))))))

(defun toml:read-string ()
  "Read string at point that surrounded by double quotation mark.
Move point to the end of read strings."
  (unless (eq ?\" (toml:get-char-at-point))
    (signal 'toml-string-error (list (point))))
  (let ((characters '())
        (char (toml:read-char)))
    (while (not (eq char ?\"))
      (when (toml:end-of-line-p)
        (signal 'toml-string-error (list (point))))
      (push (if (eq char ?\\)
                (toml:read-escaped-char)
              (toml:read-char))
            characters)
      (setq char (toml:get-char-at-point)))
    (forward-char)
    (apply 'concat (nreverse characters))))

(defun toml:read-literal-string ()
  "Read TOML literal string (enclosed in single quotes) at point."
  (unless (eq ?\' (toml:get-char-at-point))
    (signal 'toml-string-error (list (point))))
  (forward-char)
  (let ((characters '()))
    (while (not (eq (toml:get-char-at-point) ?\'))
      (when (toml:end-of-line-p)
        (signal 'toml-string-error (list (point))))
      (push (toml:read-char) characters))
    (forward-char)
    (apply #'concat (nreverse characters))))

(defun toml:read-boolean ()
  "Read boolean at point.  Return t or nil.
Move point to the end of read boolean string."
  (cond
   ((toml:search-forward "true") t)
   ((toml:search-forward "false") nil)
   (t
    (signal 'toml-boolean-error (list (point))))))

(defun toml:read-datetime ()
  "Read datetime at point.
Return time list (seconds, minutes, hour, day, month and year).
Move point to the end of read datetime string."
  (unless (toml:search-forward toml->regexp-datetime)
    (signal 'toml-datetime-error (list (point))))
  (let ((seconds (string-to-number (match-string-no-properties 6)))
        (minutes (string-to-number (match-string-no-properties 5)))
        (hour    (string-to-number (match-string-no-properties 4)))
        (day     (string-to-number (match-string-no-properties 3)))
        (month   (string-to-number (match-string-no-properties 2)))
        (year    (string-to-number (match-string-no-properties 1))))
    (list seconds minutes hour day month year)))

(defun toml:read-numeric ()
  "Read numeric (integer or float) at point.  Return numeric.
Move point to the end of read numeric string."
  (unless (toml:search-forward toml->regexp-numeric)
    (signal 'toml-numeric-error (list (point))))
  (let ((numeric (match-string-no-properties 0)))
    (if (with-temp-buffer
          (insert numeric)
          (goto-char (point-min))
          (search-forward "." nil t 2)) ;; e.g. "0.21.1" is NG
        (signal 'toml-numeric-error (list (point)))
      (string-to-number numeric))))

(defun toml:read-start-with-number ()
  "Read string that start with number at point.
Move point to the end of read string."
  (cond
   ((looking-at toml->regexp-datetime) (toml:read-datetime))
   ((looking-at toml->regexp-numeric) (toml:read-numeric))
   (t
    (signal 'toml-start-with-number-error (list (point))))))

(defun toml:read-array ()
  (unless (eq ?\[ (toml:get-char-at-point))
    (signal 'toml-array-error (list (point))))
  (mark-sexp)
  (forward-char)
  (let (elements-list char-after-read)
    (while (not (char-equal (toml:get-char-at-point) ?\]))
      (push (toml:read-value) elements-list)
      (toml:seek-readable-point)
      (setq char-after-read (toml:get-char-at-point))
      (unless (char-equal char-after-read ?\])
        (if (char-equal char-after-read ?,)
            (progn
              (forward-char)
              (toml:seek-readable-point))
          (signal 'toml-array-error (list (point))))))
    (forward-char)
    (apply #'vector (nreverse elements-list))))

(defun toml:read-inline-table ()
  (unless (eq ?{ (toml:get-char-at-point))
    (signal 'toml-inline-table-error (list (point))))
  (forward-char)
  (let (elements char-after-read)
    (while (not (char-equal (toml:get-char-at-point) ?}))
      (let ((key (toml:read-key))
            (value (toml:read-value)))
        (push `(,key . ,value) elements))
      (toml:seek-readable-point)
      (setq char-after-read (toml:get-char-at-point))
      (unless (char-equal char-after-read ?})
        (if (char-equal char-after-read ?,)
            (progn
              (forward-char)
              (toml:seek-readable-point))
          (signal 'toml-array-error (list (point))))))
    (forward-char)
    (nreverse elements)))

(defun toml:read-value ()
  (toml:seek-readable-point)
  (if (eobp) nil
    (let ((read-function (cdr (assq (toml:get-char-at-point) toml->parse-dispatch-table))))
      (if (functionp read-function)
          (funcall read-function)
        (signal 'toml-value-error (list (point)))))))

(defun toml:read-table (&optional table-history-checker)
  "Parse TOML table header and return table type and keys.
Returns a plist with :type (single or array) and :keys (list of key names).
Handles both regular tables [table.name] and array of tables
\[[table.name]].

TABLE-HISTORY-CHECKER is an optional function that takes keys and
signals an error if the table has been defined before.

Behavior differences:
- :type single (for table [xxx])
  - If no key/value pairs are defined, continue to next table
- :type array (for array of table [[xxx]])
  - Stop reading regardless of key/value presence"
  (toml:seek-readable-point)
  (let (table-keys table-type)
    (while (and (not (eobp))
                (or (null table-type) (eq table-type 'single))
                (char-equal (toml:get-char-at-point) ?\[))
      (if (toml:search-forward "\\(\\[\\{1,2\\}\\)\\([a-zA-Z][a-zA-Z0-9_\\.-]*\\)\\(\\]\\{1,2\\}\\)")
          (let* ((brackets-before (match-string-no-properties 1)) ;;  "["  in "[xxx]"
                 (brackets-after (match-string-no-properties 3))  ;;  "]"  in "[xxx]"
                 (table-string (match-string-no-properties 2)))   ;; "xxx" in "[xxx]"
            (when (string-match "\\(_\\|\\.\\)\\'" table-string)
              (signal 'toml-table-error (list (point))))
            (unless (= (length brackets-before) (length brackets-after))
              (signal 'toml-table-error (list (point))))
            (setq table-keys (split-string table-string "\\."))
            (setq table-type (if (string= brackets-before "[[") 'array 'single))
            ;; Check for table redefinition if checker is provided
            (when (and table-history-checker (eq table-type 'single))
              (funcall table-history-checker table-keys)))
        (signal 'toml-table-error (list (point))))
      (toml:seek-readable-point))
    (list :type table-type :keys table-keys)))

(defun toml:read-key ()
  (toml:seek-readable-point)
  (cond
   ((eobp) nil)
   ;; If we're at a table header, return nil (no key to read)
   ((char-equal (toml:get-char-at-point) ?\[) nil)
   ((toml:search-forward "\\([a-zA-Z][a-zA-Z0-9_-]*\\) *= *")
    (let ((key (match-string-no-properties 1)))
      (when (string-match "_\\'" key)
        (signal 'toml-key-error (list (point))))
      key))
   (t (signal 'toml-key-error (list (point))))))

(defun toml:make-table-hashes (table-keys key value hashes)
  "Add VALUE to HASHES at TABLE-KEYS + KEY path, creating nested tables.

Usage:
  (setq hash nil)
  (setq hash (toml:make-table-hashes
              \\='(\"servers\" \"alpha\") \"ip\" \"192.0.2.1\" hash))
  ;; => ((\"servers\" (\"alpha\" (\"ip\" . \"192.0.2.1\"))))
  (setq hash (toml:make-table-hashes
              \\='(\"servers\" \"alpha\") \"dc\" \"eqdc10\" hash))
  ;; => ((\"servers\" (\"alpha\" (\"dc\" . \"eqdc10\")
  ;;                          (\"ip\" . \"192.0.2.1\"))))"
  (letrec ((build-nested (lambda (hash-data keys-list val)
                           (if (null keys-list)
                               val
                             (let* ((current-key (car keys-list))
                                    (descendants (cdr keys-list))
                                    (element (assoc current-key hash-data))
                                    (children (cdr element)))
                               (setq hash-data (delete element hash-data))
                               (if descendants
                                   (let ((new-children (funcall build-nested children descendants val)))
                                     (push (cons current-key new-children) hash-data))
                                 (push (cons current-key val) hash-data))
                               hash-data)))))
    (let ((keys (append table-keys (list key))))
      (funcall build-nested hashes keys value))))

(defun toml:find-parent-array-table (keys array-table-registry)
  "Find if KEYS has a parent that is an array table.
Returns the parent array table keys if found, nil otherwise.
Only returns a PROPER parent, not the keys themselves.

Example:
  ;; If (\"fruits\") is registered as array table
  (toml:find-parent-array-table \\='(\"fruits\" \"physical\") registry)
  ;; => (\"fruits\")

  (toml:find-parent-array-table \\='(\"fruits\") registry)
  ;; => nil  ; not a proper parent, it is the same"
  (let ((result nil)
        (prefix nil)
        (keys-len (length keys)))
    (catch 'found
      (dolist (key keys)
        (setq prefix (append prefix (list key)))
        ;; Only consider proper prefixes (not the full path)
        (when (< (length prefix) keys-len)
          (let ((key-str (mapconcat 'identity prefix ".")))
            (when (assoc key-str array-table-registry)
              (setq result prefix)
              (throw 'found result))))))
    result))

(defun toml:make-array-table-hashes (array-keys hashes &optional parent-array-context)
  "Create or extend an array of tables entry at ARRAY-KEYS in HASHES.
Returns updated hashes with a new empty element added to the array.

PARENT-ARRAY-CONTEXT is a plist (:keys array-keys :index n) indicating
the parent array table context for nested array tables.

Example:
  (toml:make-array-table-hashes \\='(\"products\") nil)
  ;; => ((\"products\" . [nil]))

  (toml:make-array-table-hashes \\='(\"products\")
                                \\='((\"products\" . [nil])))
  ;; => ((\"products\" . [nil nil]))"
  (if (null array-keys)
      hashes
    (let* ((key (car array-keys))
           (rest-keys (cdr array-keys))
           (existing (assoc key hashes)))
      (cond
       ;; Nested array table: navigate into parent array's last element
       (parent-array-context
        (let* ((parent-keys (plist-get parent-array-context :keys))
               (parent-index (plist-get parent-array-context :index))
               (parent-entry (assoc (car parent-keys) hashes))
               (parent-array (cdr parent-entry)))
          (when (and parent-array (vectorp parent-array))
            (let* ((last-elem (aref parent-array parent-index))
                   (updated-elem (toml:make-array-table-hashes
                                  array-keys last-elem nil)))
              (aset parent-array parent-index updated-elem)))
          hashes))
       ;; No more nested keys - create/extend the array
       ((null rest-keys)
        (if existing
            (let ((current-val (cdr existing)))
              (if (vectorp current-val)
                  ;; Extend existing array with new empty element
                  (progn
                    (setcdr existing (vconcat current-val [nil]))
                    hashes)
                ;; Conflict: trying to redefine non-array as array table
                (signal 'toml-array-table-error (list (point)))))
          ;; Create new array with one empty element
          (cons (cons key [nil]) hashes)))
       ;; More keys to traverse - recurse
       (t
        (let* ((children (if existing (cdr existing) nil))
               (updated-children (toml:make-array-table-hashes rest-keys children nil)))
          (if existing
              (progn (setcdr existing updated-children) hashes)
            (cons (cons key updated-children) hashes))))))))

(defun toml:make-nested-array-table (parent-keys parent-index child-keys hashes)
  "Create or extend a nested array table within a parent array element.
PARENT-KEYS is the path to the parent array table.
PARENT-INDEX is the index of the current element in the parent array.
CHILD-KEYS is the remaining path for the nested array.
Returns updated hashes."
  (let* ((parent-entry (toml:assoc parent-keys hashes))
         (parent-array (cdr parent-entry)))
    (when (and parent-array (vectorp parent-array))
      (let* ((current-elem (aref parent-array parent-index))
             (child-key (car child-keys))
             (existing-child (assoc child-key current-elem)))
        (if existing-child
            ;; Extend existing nested array
            (let ((child-array (cdr existing-child)))
              (when (vectorp child-array)
                (setcdr existing-child (vconcat child-array [nil]))))
          ;; Create new nested array
          (let ((new-elem (cons (cons child-key [nil]) current-elem)))
            (aset parent-array parent-index new-elem)))))
    hashes))

(defun toml:add-to-array-table (array-keys sub-keys key value hashes array-registry)
  "Add KEY=VALUE to the last element of array table at ARRAY-KEYS.
SUB-KEYS are additional sub-table keys within the array element.
Returns updated hashes.

Example:
  ;; For [[products]] with name = \"Hammer\"
  (toml:add-to-array-table \\='(\"products\") nil
                           \"name\" \"Hammer\" hashes registry)

  ;; For [[fruits]] with [fruits.physical] color = \"red\"
  (toml:add-to-array-table \\='(\"fruits\") \\='(\"physical\")
                           \"color\" \"red\" hashes registry)

  ;; For [[fruits.varieties]] with name = \"red delicious\"
  ;; array-keys = (\"fruits\" \"varieties\"), parent is (\"fruits\")
  (toml:add-to-array-table \\='(\"fruits\" \"varieties\") nil
                           \"name\" \"red delicious\" hashes registry)"
  (let ((parent-array (toml:find-parent-array-table array-keys array-registry)))
    (if parent-array
        ;; Nested array table: navigate through parent first
        (let* ((parent-key-str (mapconcat 'identity parent-array "."))
               (parent-index (cdr (assoc parent-key-str array-registry)))
               (parent-entry (toml:assoc parent-array hashes))
               (parent-vec (cdr parent-entry)))
          (when (and parent-vec (vectorp parent-vec) parent-index)
            (let* ((parent-elem (aref parent-vec parent-index))
                   (child-keys (nthcdr (length parent-array) array-keys))
                   (child-key (car child-keys))
                   (child-entry (assoc child-key parent-elem))
                   (child-array (cdr child-entry))
                   (child-key-str (mapconcat 'identity array-keys "."))
                   (child-index (cdr (assoc child-key-str array-registry))))
              (when (and child-array (vectorp child-array) child-index)
                (let* ((current-elem (aref child-array child-index))
                       (updated-elem (toml:make-table-hashes sub-keys key value current-elem)))
                  (aset child-array child-index updated-elem))))))
      ;; Top-level array table
      (let* ((array-key-str (mapconcat 'identity array-keys "."))
             (index (cdr (assoc array-key-str array-registry)))
             (entry (toml:assoc array-keys hashes))
             (array-vec (cdr entry)))
        (when (and array-vec (vectorp array-vec) index)
          (let* ((current-elem (aref array-vec index))
                 (updated-elem (toml:make-table-hashes sub-keys key value current-elem)))
            (aset array-vec index updated-elem)))))
    hashes))

(defun toml:read ()
  "Parse and return the TOML object following point."
  (let (current-table
        current-array-table      ; Current array table keys (e.g., ("products"))
        current-array-sub-keys   ; Sub-table keys within array element
        current-key
        current-value
        hashes
        table-history
        array-table-registry)    ; Alist of ("key.path" . last-index)
    (while (not (eobp))
      (toml:seek-readable-point)

      ;; Parse table header with history checker
      (cl-destructuring-bind (&key type keys)
          (toml:read-table
           (lambda (keys)
             ;; Skip history check for sub-tables of array tables
             (unless (toml:find-parent-array-table keys array-table-registry)
               (when (member keys table-history)
                 (signal 'toml-redefine-table-error (list (point))))
               (push keys table-history))))
        (cond
         ((eq type 'single)
          ;; Check if this table conflicts with an existing array table
          (let ((key-str (mapconcat 'identity keys ".")))
            (when (assoc key-str array-table-registry)
              ;; This table name conflicts with an existing array table
              (signal 'toml-array-table-error (list (point)))))
          ;; Check if this is a sub-table of an array table
          (let ((parent-array (toml:find-parent-array-table keys array-table-registry)))
            (if parent-array
                ;; This is a sub-table within an array element
                (progn
                  (setq current-array-table parent-array)
                  (setq current-array-sub-keys (nthcdr (length parent-array) keys))
                  (setq current-table nil))
              ;; Regular table
              (setq current-table keys)
              (setq current-array-table nil)
              (setq current-array-sub-keys nil))))

         ((eq type 'array)
          ;; Array of tables
          ;; Check if trying to append to a statically defined array
          (let* ((key-str (mapconcat 'identity keys "."))
                 (existing-entry (toml:assoc keys hashes)))
            (when (and existing-entry
                       (vectorp (cdr existing-entry))
                       (not (assoc key-str array-table-registry)))
              ;; This is a static array, not an array table
              (signal 'toml-array-table-error (list (point)))))
          ;; Check if this is a nested array table (parent is also an array table)
          (let ((parent-array (toml:find-parent-array-table keys array-table-registry)))
            (if parent-array
                ;; Nested array table: add to parent's last element
                (let* ((parent-key-str (mapconcat 'identity parent-array "."))
                       (parent-index (cdr (assoc parent-key-str array-table-registry)))
                       (child-keys (nthcdr (length parent-array) keys)))
                  ;; Create/extend the nested array within parent's element
                  (setq hashes (toml:make-nested-array-table
                                parent-array parent-index child-keys hashes))
                  ;; Register the nested array table
                  (let* ((key-str (mapconcat 'identity keys "."))
                         (existing (assoc key-str array-table-registry)))
                    (if existing
                        (setcdr existing (1+ (cdr existing)))
                      (push (cons key-str 0) array-table-registry)))
                  (setq current-array-table keys)
                  (setq current-array-sub-keys nil)
                  (setq current-table nil))
              ;; Top-level array table
              (setq hashes (toml:make-array-table-hashes keys hashes nil))
              (let* ((key-str (mapconcat 'identity keys "."))
                     (existing (assoc key-str array-table-registry)))
                (if existing
                    (setcdr existing (1+ (cdr existing)))
                  (push (cons key-str 0) array-table-registry))
                ;; Reset child array table indices when parent gets new element
                (let ((prefix (concat key-str ".")))
                  (dolist (entry array-table-registry)
                    (when (string-prefix-p prefix (car entry))
                      (setcdr entry -1)))))  ; -1 so next increment makes it 0
              (setq current-array-table keys)
              (setq current-array-sub-keys nil)
              (setq current-table nil))))

         (t
          ;; No table header, continue with current context
          nil)))

      ;; Validate table doesn't conflict with existing keys
      (when current-table
        (let ((elm (toml:assoc current-table hashes)))
          (when (and elm (not (toml:alistp (cdr elm))))
            (signal 'toml-redefine-key-error (list (point))))))

      ;; Read key-value pair
      (setq current-key (toml:read-key))
      (when current-key
        ;; Check for key redefinition
        (let ((full-path (if current-array-table
                             ;; For array tables, we don't check global redefinition
                             nil
                           (append current-table (list current-key)))))
          (when (and full-path (toml:assoc full-path hashes))
            (signal 'toml-redefine-key-error (list (point)))))

        (setq current-value (toml:read-value))

        ;; Add to appropriate structure
        (if current-array-table
            ;; Add to array table element
            (setq hashes (toml:add-to-array-table current-array-table
                                                   current-array-sub-keys
                                                   current-key
                                                   current-value
                                                   hashes
                                                   array-table-registry))
          ;; Add to regular table
          (setq hashes (toml:make-table-hashes current-table
                                               current-key
                                               current-value
                                               hashes))))

      (toml:seek-readable-point))
    hashes))

(defun toml:read-from-string (string)
  "Read the TOML object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (toml:read)))

(defun toml:read-from-file (file)
  "Read the TOML object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (toml:read)))

(provide 'toml)

;;; toml.el ends here
