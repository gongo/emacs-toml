;;; toml.el --- TOML (Tom's Obvious, Minimal Language) parser -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-toml
;; Keywords: toml parser
;; Version: 1.1.0.0

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

(defconst toml->escape-sequence-alist
  '((?b . ?\b)    ; backspace       (U+0008)
    (?t . ?\t)    ; tab             (U+0009)
    (?n . ?\n)    ; linefeed        (U+000A)
    (?f . ?\f)    ; form feed       (U+000C)
    (?r . ?\r)    ; carriage return (U+000D)
    (?e . ?\e)    ; escape          (U+001B)
    (?\" . ?\")   ; quote           (U+0022)
    (?\\ . ?\\))  ; backslash       (U+005C)
  "Alist mapping TOML escape characters to their actual values.
Excludes \\uXXXX which is handled separately in `toml:read-escaped-char'.")

(defconst toml->parse-dispatch-table
  (let ((table
         '((?t  . toml:read-boolean)
           (?f  . toml:read-boolean)
           (?i  . toml:read-special-float)
           (?n  . toml:read-special-float)
           (?\[ . toml:read-array)
           (?{  . toml:read-inline-table)
           (?\" . toml:read-string)
           (?\' . toml:read-literal-string))))
    (mapc (lambda (char)
            (push (cons char 'toml:read-start-with-number) table))
          '(?+ ?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table))

(defconst toml->regexp-datetime
  "\
\\([0-9]\\{4\\}\\)-\
\\(0[1-9]\\|1[0-2]\\)-\
\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)[Tt ]\
\\([0-1][0-9]\\|2[0-3]\\):\
\\([0-5][0-9]\\)\
\\(?::\\([0-5][0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?\
\\([Zz]\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "Regular expression for RFC 3339 datetime with timezone and fractional seconds.")

(defconst toml->regexp-local-datetime
  "\
\\([0-9]\\{4\\}\\)-\
\\(0[1-9]\\|1[0-2]\\)-\
\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)[Tt ]\
\\([0-1][0-9]\\|2[0-3]\\):\
\\([0-5][0-9]\\)\
\\(?::\\([0-5][0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
  "Regular expression for local date-time (no timezone).")

(defconst toml->regexp-local-date
  "\
\\([0-9]\\{4\\}\\)-\
\\(0[1-9]\\|1[0-2]\\)-\
\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)"
  "Regular expression for local date (date only, no time).")

(defconst toml->regexp-local-time
  "\
\\([0-1][0-9]\\|2[0-3]\\):\
\\([0-5][0-9]\\)\
\\(?::\\([0-5][0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?\\)?"
  "Regular expression for local time (time only, no date).")

(defconst toml->regexp-hex
  "\\(0x[0-9a-fA-F]+\\(?:_[0-9a-fA-F]+\\)*\\)"
  "Regular expression for hexadecimal integer literals.")

(defconst toml->regexp-oct
  "\\(0o[0-7]+\\(?:_[0-7]+\\)*\\)"
  "Regular expression for octal integer literals.")

(defconst toml->regexp-bin
  "\\(0b[01]+\\(?:_[01]+\\)*\\)"
  "Regular expression for binary integer literals.")

(defconst toml->regexp-numeric
  "\\([+-]?[0-9][_0-9]*[.0-9eE+_-]*\\)"
  "Regular expression for capturing numeric-like strings.")

(defconst toml->regexp-numeric-strict
  "^[+-]?[0-9]+\\(?:_[0-9]+\\)*\\(?:\\.\\(?:[0-9]+\\(?:_[0-9]+\\)*\\)\\)?\\(?:[eE][+-]?[0-9]+\\(?:_[0-9]+\\)*\\)?$"
  "Strict regular expression for validating numeric format.")

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

(put 'toml-comment-error 'error-message "Bad comment")
(put 'toml-comment-error 'error-conditions
     '(toml-comment-error toml-error error))

(put 'toml-inline-table-error 'error-message "Bad inline table")
(put 'toml-inline-table-error 'error-conditions
     '(toml-inline-table-error toml-error error))

(put 'toml-inline-table-immutable-error 'error-message "Inline table is immutable")
(put 'toml-inline-table-immutable-error 'error-conditions
     '(toml-inline-table-immutable-error toml-error error))

(put 'toml-encoding-error 'error-message "Bad encoding")
(put 'toml-encoding-error 'error-conditions
     '(toml-encoding-error toml-error error))

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

(defsubst toml:array-table-elem (elem)
  "Return ELEM as an alist for array table operations.
Converts the :toml-empty-table sentinel to nil (empty alist)."
  (if (eq elem :toml-empty-table) nil elem))

(defun toml:alistp (alist)
  "Return t if ALIST is a list of association lists, nil otherwise."
  (when (listp alist)
    (catch 'break
      (dolist (al alist)
        (unless (consp al) (throw 'break nil)))
      t)))

(defun toml:collect-inline-table-paths (prefix alist)
  "Collect PREFIX and all nested sub-table paths within ALIST."
  (let ((result (list prefix)))
    (dolist (entry alist)
      (when (toml:alistp (cdr entry))
        (setq result (append result
                             (toml:collect-inline-table-paths
                              (append prefix (list (car entry)))
                              (cdr entry))))))
    result))

(defun toml:check-inline-table-conflict (path registry)
  "Signal error if PATH equals or extends any path in REGISTRY."
  (dolist (registered registry)
    (when (and (<= (length registered) (length path))
               (equal registered (butlast path (- (length path) (length registered)))))
      (signal 'toml-inline-table-immutable-error (list path)))))

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
    (forward-char) ; skip '#'
    (while (and (not (eobp)) (not (toml:end-of-line-p)))
      (when (toml:control-char-p (toml:get-char-at-point))
        (signal 'toml-comment-error (list (point))))
      (forward-char))
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
  (let ((case-fold-search nil))
    (when (looking-at regexp)
      (forward-char (length (match-string-no-properties 0)))
      t)))

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
         (mapped (assq char toml->escape-sequence-alist)))
    (cond
     (mapped (char-to-string (cdr mapped)))
     ((and (eq char ?x)
           (toml:search-forward "[0-9A-Fa-f]\\{2\\}"))
      (char-to-string (string-to-number (match-string 0) 16)))
     ((and (eq char ?u)
           (toml:search-forward "[0-9A-Fa-f]\\{4\\}"))
      (let ((code-point (string-to-number (match-string 0) 16)))
        (when (or (and (>= code-point #xD800) (<= code-point #xDFFF))
                  (> code-point #x10FFFF))
          (signal 'toml-string-unicode-escape-error (list (point))))
        (char-to-string code-point)))
     ((and (eq char ?U)
           (toml:search-forward "[0-9A-Fa-f]\\{8\\}"))
      (let ((code-point (string-to-number (match-string 0) 16)))
        (when (or (and (>= code-point #xD800) (<= code-point #xDFFF))
                  (> code-point #x10FFFF))
          (signal 'toml-string-unicode-escape-error (list (point))))
        (char-to-string code-point)))
     (t (signal 'toml-string-unicode-escape-error (list (point)))))))

(defun toml:control-char-p (char)
  "Return non-nil if CHAR is a control character forbidden in TOML strings.
Control characters are U+0000 to U+001F (except TAB U+0009) and U+007F."
  (or (and (>= char #x00) (<= char #x08))
      (and (>= char #x0A) (<= char #x1F))
      (= char #x7F)))

(defun toml:read-multiline-basic-string ()
  "Read multiline basic string (enclosed in \"\"\") at point.
Handles escape sequences, line-ending backslash continuation,
and trims immediate newline after opening delimiter."
  ;; Skip the opening """
  (forward-char 3)
  ;; Trim immediate newline after opening delimiter
  (when (eq (toml:get-char-at-point) ?\n)
    (forward-char))
  (let ((characters '()))
    (while (not (looking-at "\"\"\""))
      (when (eobp)
        (signal 'toml-string-error (list (point))))
      (cond
       ;; Line-ending backslash: trim the backslash, newline, and following whitespace
       ((looking-at "\\\\[ \t]*\n[ \t\n]*")
        (goto-char (match-end 0)))
       (t
        (let ((char (toml:get-char-at-point)))
          (when (and (toml:control-char-p char)
                     (not (eq char ?\n)))
            (signal 'toml-string-error (list (point))))
          ;; Regular escape sequence or regular character
          (if (eq char ?\\)
              (push (toml:read-escaped-char) characters)
            (push (toml:read-char) characters))))))
    ;; Up to 2 extra quotes allowed before closing """
    (let ((quote-count 0))
      (save-excursion
        (while (and (not (eobp)) (eq (char-after) ?\"))
          (setq quote-count (1+ quote-count))
          (forward-char)))
      (when (>= quote-count 6)
        (signal 'toml-string-error (list (point))))
      (dotimes (_ (- quote-count 3))
        (push "\"" characters))
      (forward-char quote-count))
    (apply #'concat (nreverse characters))))

(defun toml:read-string ()
  "Read string at point that surrounded by double quotation mark.
Move point to the end of read strings."
  (unless (eq ?\" (toml:get-char-at-point))
    (signal 'toml-string-error (list (point))))
  ;; Check for multiline string (""")
  (if (looking-at "\"\"\"")
      (toml:read-multiline-basic-string)
    (forward-char)  ; skip opening "
    (let ((characters '()))
      (while (not (eq (toml:get-char-at-point) ?\"))
        (when (toml:end-of-line-p)
          (signal 'toml-string-error (list (point))))
        (let ((char (toml:get-char-at-point)))
          (when (toml:control-char-p char)
            (signal 'toml-string-error (list (point))))
          (if (eq char ?\\)
              (push (toml:read-escaped-char) characters)
            (push (toml:read-char) characters))))
      (forward-char)  ; skip closing "
      (apply #'concat (nreverse characters)))))

(defun toml:read-multiline-literal-string ()
  "Read multiline literal string (enclosed in ''') at point.
No escape processing. Trims immediate newline after opening delimiter."
  ;; Skip the opening '''
  (forward-char 3)
  ;; Trim immediate newline after opening delimiter
  (when (eq (toml:get-char-at-point) ?\n)
    (forward-char))
  (let ((characters '()))
    (while (not (looking-at "'''"))
      (when (eobp)
        (signal 'toml-string-error (list (point))))
      (let ((char (toml:get-char-at-point)))
        (when (and (toml:control-char-p char)
                   (not (eq char ?\n)))
          (signal 'toml-string-error (list (point)))))
      (push (toml:read-char) characters))
    ;; Up to 2 extra quotes allowed before closing '''
    (let ((quote-count 0))
      (save-excursion
        (while (and (not (eobp)) (eq (char-after) ?\'))
          (setq quote-count (1+ quote-count))
          (forward-char)))
      (when (>= quote-count 6)
        (signal 'toml-string-error (list (point))))
      (dotimes (_ (- quote-count 3))
        (push "'" characters))
      (forward-char quote-count))
    (apply #'concat (nreverse characters))))

(defun toml:read-literal-string ()
  "Read TOML literal string (enclosed in single quotes) at point."
  (unless (eq ?\' (toml:get-char-at-point))
    (signal 'toml-string-error (list (point))))
  ;; Check for multiline literal string (''')
  (if (looking-at "'''")
      (toml:read-multiline-literal-string)
    (forward-char)
    (let ((characters '()))
      (while (not (eq (toml:get-char-at-point) ?\'))
        (when (toml:end-of-line-p)
          (signal 'toml-string-error (list (point))))
        (let ((char (toml:get-char-at-point)))
          (when (toml:control-char-p char)
            (signal 'toml-string-error (list (point)))))
        (push (toml:read-char) characters))
      (forward-char)
      (apply #'concat (nreverse characters)))))

(defun toml:validate-date (year month day)
  "Validate that YEAR-MONTH-DAY is a valid date.
Signal `toml-datetime-error' if the day exceeds the month's maximum."
  (let ((max-day (pcase month
                   (2 (if (date-leap-year-p year) 29 28))
                   ((or 4 6 9 11) 30)
                   (_ 31))))
    (when (> day max-day)
      (signal 'toml-datetime-error (list (point))))))

(defun toml:read-boolean ()
  "Read boolean at point.  Return t or nil.
Move point to the end of read boolean string."
  (cond
   ((toml:search-forward "true") t)
   ((toml:search-forward "false") nil)
   (t
    (signal 'toml-boolean-error (list (point))))))

(defun toml:read-datetime ()
  "Read RFC 3339 datetime at point.
Return alist with keys: year, month, day, hour, minute, second,
fraction, timezone.
Move point to the end of read datetime string."
  (unless (toml:search-forward toml->regexp-datetime)
    (signal 'toml-datetime-error (list (point))))
  (let ((year     (string-to-number (match-string-no-properties 1)))
        (month    (string-to-number (match-string-no-properties 2)))
        (day      (string-to-number (match-string-no-properties 3)))
        (hour     (string-to-number (match-string-no-properties 4)))
        (minute   (string-to-number (match-string-no-properties 5)))
        (second   (let ((s (match-string-no-properties 6))) (if s (string-to-number s) 0)))
        (fraction (match-string-no-properties 7))  ; optional
        (timezone (let ((tz (match-string-no-properties 8)))
                    (if (and tz (string= (upcase tz) "Z")) "Z" tz))))
    (when (and timezone (not (string= timezone "Z")))
      (let ((tz-hour (string-to-number (substring timezone 1 3)))
            (tz-minute (string-to-number (substring timezone 4 6))))
        (unless (and (<= 0 tz-hour 23) (<= 0 tz-minute 59))
          (signal 'toml-datetime-error (list (point))))))
    (toml:validate-date year month day)
    `((year . ,year)
      (month . ,month)
      (day . ,day)
      (hour . ,hour)
      (minute . ,minute)
      (second . ,second)
      (fraction . ,(when fraction (string-to-number (concat "0." fraction))))
      (timezone . ,timezone))))

(defun toml:read-local-datetime ()
  "Read local date-time (no timezone) at point.
Return alist with keys: year, month, day, hour, minute, second, fraction.
Move point to the end of read datetime string."
  (unless (toml:search-forward toml->regexp-local-datetime)
    (signal 'toml-datetime-error (list (point))))
  (let ((year     (string-to-number (match-string-no-properties 1)))
        (month    (string-to-number (match-string-no-properties 2)))
        (day      (string-to-number (match-string-no-properties 3)))
        (hour     (string-to-number (match-string-no-properties 4)))
        (minute   (string-to-number (match-string-no-properties 5)))
        (second   (let ((s (match-string-no-properties 6))) (if s (string-to-number s) 0)))
        (fraction (match-string-no-properties 7)))
    (toml:validate-date year month day)
    `((year . ,year)
      (month . ,month)
      (day . ,day)
      (hour . ,hour)
      (minute . ,minute)
      (second . ,second)
      (fraction . ,(when fraction (string-to-number (concat "0." fraction)))))))

(defun toml:read-local-date ()
  "Read local date at point.
Return alist with keys: year, month, day.
Move point to the end of read date string."
  (unless (toml:search-forward toml->regexp-local-date)
    (signal 'toml-datetime-error (list (point))))
  (let ((year  (string-to-number (match-string-no-properties 1)))
        (month (string-to-number (match-string-no-properties 2)))
        (day   (string-to-number (match-string-no-properties 3))))
    (toml:validate-date year month day)
    `((year . ,year)
      (month . ,month)
      (day . ,day))))

(defun toml:read-local-time ()
  "Read local time at point.
Return alist with keys: hour, minute, second, fraction.
Move point to the end of read time string."
  (unless (toml:search-forward toml->regexp-local-time)
    (signal 'toml-datetime-error (list (point))))
  (let ((hour     (string-to-number (match-string-no-properties 1)))
        (minute   (string-to-number (match-string-no-properties 2)))
        (second   (let ((s (match-string-no-properties 3))) (if s (string-to-number s) 0)))
        (fraction (match-string-no-properties 4)))
    `((hour . ,hour)
      (minute . ,minute)
      (second . ,second)
      (fraction . ,(when fraction (string-to-number (concat "0." fraction)))))))

(defun toml:read-numeric ()
  "Read numeric (integer or float) at point.  Return numeric.
Move point to the end of read numeric string."
  (cond
   ;; Reject uppercase prefixes: 0X, 0O, 0B
   ((let ((case-fold-search nil))
      (looking-at "0[XOB]"))
    (signal 'toml-numeric-error (list (point))))
   ;; Reject underscore after prefix: 0x_, 0o_, 0b_
   ((looking-at "0[xob]_")
    (signal 'toml-numeric-error (list (point))))
   ;; Hexadecimal: 0xDEADBEEF
   ((toml:search-forward toml->regexp-hex)
    (let ((s (replace-regexp-in-string "_" "" (match-string-no-properties 1))))
      (string-to-number (substring s 2) 16)))
   ;; Octal: 0o755
   ((toml:search-forward toml->regexp-oct)
    (let ((s (replace-regexp-in-string "_" "" (match-string-no-properties 1))))
      (string-to-number (substring s 2) 8)))
   ;; Binary: 0b11010110
   ((toml:search-forward toml->regexp-bin)
    (let ((s (replace-regexp-in-string "_" "" (match-string-no-properties 1))))
      (string-to-number (substring s 2) 2)))
   ;; Regular decimal numeric
   ((toml:search-forward toml->regexp-numeric)
    (let ((numeric-str (match-string-no-properties 0)))
      ;; Reject leading zeros in decimal numbers (e.g., 01, 00, 03.14)
      ;; Leading zero is only valid for: 0, 0.x, 0ex
      (let ((abs-str (if (string-match-p "^[+-]" numeric-str)
                         (substring numeric-str 1)
                       numeric-str)))
        (when (and (> (length abs-str) 1)
                   (eq (aref abs-str 0) ?0)
                   (let ((c (aref abs-str 1)))
                     (and (not (eq c ?.))
                          (not (memq c '(?e ?E))))))
          (signal 'toml-numeric-error (list (point)))))
      ;; Two-stage validation:
      ;; 1. toml->regexp-numeric (loose) - greedily captures all numeric-like
      ;;    characters to ensure invalid trailing chars (e.g., "1.1.1") are
      ;;    included in the match rather than left unparsed.
      ;; 2. toml->regexp-numeric-strict - validates the captured string has
      ;;    correct format (e.g., rejects "1e", "1.", "1.1.1").
      (unless (string-match-p toml->regexp-numeric-strict numeric-str)
        (signal 'toml-numeric-error (list (point))))
      (string-to-number (replace-regexp-in-string "_" "" numeric-str))))
   (t
    (signal 'toml-numeric-error (list (point))))))

(defun toml:read-special-float ()
  "Read special float value (inf or nan) at point.
Return the corresponding Emacs Lisp float value."
  (cond
   ((toml:search-forward "inf") 1.0e+INF)
   ((toml:search-forward "nan") 0.0e+NaN)
   (t (signal 'toml-numeric-error (list (point))))))

(defun toml:read-start-with-number ()
  "Read string that start with number at point.
Move point to the end of read string."
  (cond
   ;; +inf, -inf, +nan, -nan
   ((looking-at "[+-]\\(inf\\|nan\\)")
    (let ((sign (toml:read-char t)))
      (let ((val (toml:read-special-float)))
        (if (eq sign ?-)
            (- val)
          val))))
   ;; 0x, 0o, 0b prefixed integers - skip datetime check
   ((looking-at "0[xob]") (toml:read-numeric))
   ((looking-at toml->regexp-datetime) (toml:read-datetime))
   ((looking-at toml->regexp-local-datetime) (toml:read-local-datetime))
   ((looking-at toml->regexp-local-date) (toml:read-local-date))
   ((looking-at toml->regexp-local-time) (toml:read-local-time))
   ((looking-at toml->regexp-numeric) (toml:read-numeric))
   (t
    (signal 'toml-start-with-number-error (list (point))))))

(defun toml:read-array ()
  (unless (eq ?\[ (toml:get-char-at-point))
    (signal 'toml-array-error (list (point))))
  (mark-sexp)
  (forward-char)
  (toml:seek-readable-point)
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

(defun toml:merge-alists (base new)
  "Recursively merge alist NEW into alist BASE.
For keys present in both, if both values are alists, merge recursively.
Otherwise the NEW value takes precedence."
  (dolist (entry new)
    (let* ((key (car entry))
           (val (cdr entry))
           (existing (assoc key base)))
      (if (and existing (toml:alistp (cdr existing)) (toml:alistp val))
          (setcdr existing (toml:merge-alists (cdr existing) val))
        (push entry base))))
  base)

(defun toml:read-inline-table ()
  (unless (eq ?{ (toml:get-char-at-point))
    (signal 'toml-inline-table-error (list (point))))
  (forward-char)
  (toml:seek-readable-point)
  (let (elements char-after-read
		 defined-paths implicit-paths inline-table-paths)
    (while (not (char-equal (toml:get-char-at-point) ?}))
      (let* ((key-segments (toml:read-key))
             (value (toml:read-value))
             (full-path key-segments))
        ;; Validation 1: exact duplicate
        (when (member full-path defined-paths)
          (signal 'toml-redefine-key-error (list full-path)))
        ;; Validation 2: implicit table overwrite
        ;; (full-path is a proper prefix of an existing defined-path)
        (dolist (dp defined-paths)
          (when (and (< (length full-path) (length dp))
                     (equal full-path (butlast dp (- (length dp) (length full-path)))))
            (signal 'toml-redefine-key-error (list full-path))))
        ;; Validation 3: inline table immutability
        ;; (a proper prefix of full-path is an inline-table-path)
        (dolist (itp inline-table-paths)
          (when (and (< (length itp) (length full-path))
                     (equal itp (butlast full-path (- (length full-path) (length itp)))))
            (signal 'toml-inline-table-immutable-error (list full-path))))
        ;; Validation 4: nesting into non-table scalar
        ;; (a proper prefix of full-path is in defined-paths but not in
        ;;  implicit-paths or inline-table-paths)
        (dolist (dp defined-paths)
          (when (and (< (length dp) (length full-path))
                     (equal dp (butlast full-path (- (length full-path) (length dp))))
                     (not (member dp implicit-paths))
                     (not (member dp inline-table-paths)))
            (signal 'toml-redefine-key-error (list full-path))))
        ;; Register paths
        (push full-path defined-paths)
        ;; Register intermediate segments as implicit paths
        (when (> (length key-segments) 1)
          (let ((prefix nil))
            (dolist (seg (butlast key-segments))
              (setq prefix (append prefix (list seg)))
              (unless (member prefix implicit-paths)
                (push prefix implicit-paths)))))
        ;; Register inline table value paths
        (when (or (toml:alistp value) (eq value :toml-empty-table))
          (push full-path inline-table-paths))
        ;; Build elements
        (if (= 1 (length key-segments))
            ;; Simple key: traditional (key . value)
            (push `(,(car key-segments) . ,value) elements)
          ;; Dotted key: build nested structure
          (let* ((table-path (butlast key-segments))
                 (leaf-key (car (last key-segments)))
                 (nested (toml:make-table-hashes table-path leaf-key value nil)))
            ;; Merge with existing elements (for cases like x.y=1, x.z=2)
            (dolist (entry nested)
              (let ((existing (assoc (car entry) elements)))
                (if (and existing (toml:alistp (cdr existing)) (toml:alistp (cdr entry)))
                    (setcdr existing (toml:merge-alists (cdr existing) (cdr entry)))
                  (push entry elements)))))))
      (toml:seek-readable-point)
      (setq char-after-read (toml:get-char-at-point))
      (unless (char-equal char-after-read ?})
        (if (char-equal char-after-read ?,)
            (progn
              (forward-char)
              (toml:seek-readable-point))
          (signal 'toml-inline-table-error (list (point))))))
    (forward-char)
    (if elements (nreverse elements) :toml-empty-table)))

(defun toml:ensure-value-on-same-line ()
  "Ensure that a value starts on the same line after `='.
Only skips spaces and tabs; signals `toml-key-error' if EOL or EOB."
  (skip-chars-forward " \t")
  (when (or (eobp)
	    (toml:end-of-line-p)
            (char-equal (toml:get-char-at-point) ?#))
    (signal 'toml-key-error (list (point)))))

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
      ;; Determine if this is [key] or [[key]]
      (forward-char) ;; skip first [
      (let ((is-array (and (not (eobp))
                           (char-equal (toml:get-char-at-point) ?\[))))
        (when is-array (forward-char)) ;; skip second [
        (skip-chars-forward " \t")
        ;; Read first key segment
        (let ((segments (list (toml:read-table-key-segment))))
          ;; Read remaining dotted segments
          (let ((continue t))
            (while continue
              (skip-chars-forward " \t")
              (if (and (not (eobp))
                       (char-equal (toml:get-char-at-point) ?.))
                  (progn
                    (forward-char) ;; skip dot
                    (skip-chars-forward " \t")
                    (push (toml:read-table-key-segment) segments))
                (setq continue nil))))
          (skip-chars-forward " \t")
          ;; Consume closing bracket(s) and verify match
          (if is-array
              (if (and (not (eobp))
                       (char-equal (toml:get-char-at-point) ?\])
                       (progn (forward-char) t)
                       (not (eobp))
                       (char-equal (toml:get-char-at-point) ?\]))
                  (forward-char)
                (signal 'toml-table-error (list (point))))
            (if (and (not (eobp))
                     (char-equal (toml:get-char-at-point) ?\]))
                (forward-char)
              (signal 'toml-table-error (list (point)))))
          (setq table-keys (nreverse segments))
          (setq table-type (if is-array 'array 'single))
          ;; After table header, ensure no extra tokens on the same line
          (skip-chars-forward " \t")
          (unless (or (eobp) (toml:end-of-line-p) (eq (toml:get-char-at-point) ?#))
            (signal 'toml-table-error (list (point))))
          ;; Check for table redefinition if checker is provided
          (when (and table-history-checker (eq table-type 'single))
            (funcall table-history-checker table-keys))))
      (toml:seek-readable-point))
    (list :type table-type :keys table-keys)))

(defun toml:read-table-key-segment ()
  "Read a single key segment inside a table header.
Returns the key string, or signals toml-table-error."
  (condition-case err
      (cond
       ((eobp) (signal 'toml-table-error (list (point))))
       ((char-equal (toml:get-char-at-point) ?\")
        (when (looking-at "\"\"\"")
          (signal 'toml-table-error (list (point))))
        (toml:read-string))
       ((char-equal (toml:get-char-at-point) ?\')
        (when (looking-at "'''")
          (signal 'toml-table-error (list (point))))
        (toml:read-literal-string))
       ((toml:search-forward "\\([A-Za-z0-9_-]+\\)")
        (match-string-no-properties 1))
       (t (signal 'toml-table-error (list (point)))))
    (toml-string-error
     (signal 'toml-table-error (cdr err)))))

(defun toml:read-key-segment ()
  "Read a single key segment at point.
Returns the key string, or nil if at eob or table header."
  (condition-case err
      (cond
       ((eobp) nil)
       ((char-equal (toml:get-char-at-point) ?\[) nil)
       ((char-equal (toml:get-char-at-point) ?\")
        (when (looking-at "\"\"\"")
          (signal 'toml-key-error (list (point))))
        (toml:read-string))
       ((char-equal (toml:get-char-at-point) ?\')
        (when (looking-at "'''")
          (signal 'toml-key-error (list (point))))
        (toml:read-literal-string))
       ((toml:search-forward "\\([A-Za-z0-9_-]+\\)")
        (match-string-no-properties 1))
       (t (signal 'toml-key-error (list (point)))))
    (toml-string-error
     (signal 'toml-key-error (cdr err)))))

(defun toml:read-key ()
  "Read a key at point, including dotted keys.
Returns a list of key segments (e.g., (\"name\") or (\"physical\" \"color\")),
or nil if no key is found."
  (toml:seek-readable-point)
  (let ((first-segment (toml:read-key-segment)))
    (when first-segment
      (let ((segments (list first-segment)))
        ;; Read additional dot-separated segments
        (while (progn
                 (skip-chars-forward " \t")
                 (and (not (eobp))
                      (eq (toml:get-char-at-point) ?.)))
          (forward-char) ; skip the dot
          (skip-chars-forward " \t")
          (let ((seg (toml:read-key-segment)))
            (unless seg
              (signal 'toml-key-error (list (point))))
            (push seg segments)))
        (setq segments (nreverse segments))
        ;; Expect " = " after the key
        (unless (toml:search-forward " *= *")
          (signal 'toml-key-error (list (point))))
        segments))))

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
    (dolist (key keys)
      (setq prefix (append prefix (list key)))
      ;; Only consider proper prefixes (not the full path)
      (when (< (length prefix) keys-len)
        (let ((key-str (mapconcat 'identity prefix ".")))
          (when (assoc key-str array-table-registry)
            (setq result (copy-sequence prefix))))))
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
                                \\=`((\"products\" . ,(vector nil))))
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
            (let* ((last-elem (toml:array-table-elem (aref parent-array parent-index)))
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
                    (setcdr existing (vconcat current-val (vector :toml-empty-table)))
                    hashes)
                ;; Conflict: trying to redefine non-array as array table
                (signal 'toml-array-table-error (list (point)))))
          ;; Create new array with one empty element
          (cons (cons key (vector :toml-empty-table)) hashes)))
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
      (let* ((current-elem (toml:array-table-elem (aref parent-array parent-index)))
             (child-key (car child-keys))
             (existing-child (assoc child-key current-elem)))
        (if existing-child
            ;; Extend existing nested array
            (let ((child-array (cdr existing-child)))
              (when (vectorp child-array)
                (setcdr existing-child (vconcat child-array (vector :toml-empty-table)))))
          ;; Create new nested array
          (let ((new-elem (cons (cons child-key (vector :toml-empty-table)) current-elem)))
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
            (let* ((parent-elem (toml:array-table-elem (aref parent-vec parent-index)))
                   (child-keys (nthcdr (length parent-array) array-keys))
                   (child-key (car child-keys))
                   (child-entry (assoc child-key parent-elem))
                   (child-array (cdr child-entry))
                   (child-key-str (mapconcat 'identity array-keys "."))
                   (child-index (cdr (assoc child-key-str array-registry))))
              (when (and child-array (vectorp child-array) child-index)
                (let* ((current-elem (toml:array-table-elem (aref child-array child-index)))
                       (updated-elem (toml:make-table-hashes sub-keys key value current-elem)))
                  (aset child-array child-index updated-elem))))))
      ;; Top-level array table
      (let* ((array-key-str (mapconcat 'identity array-keys "."))
             (index (cdr (assoc array-key-str array-registry)))
             (entry (toml:assoc array-keys hashes))
             (array-vec (cdr entry)))
        (when (and array-vec (vectorp array-vec) index)
          (let* ((current-elem (toml:array-table-elem (aref array-vec index)))
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
        array-table-registry     ; Alist of ("key.path" . last-index)
        inline-table-registry    ; List of paths defined by inline tables
        scalar-key-registry      ; List of key paths that hold scalar values
        explicit-table-registry) ; List of key paths explicitly defined by [table] headers
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
               (push keys table-history)
               (push keys explicit-table-registry))))
        (cond
         ((eq type 'single)
          ;; Check if this table conflicts with an inline table
          (toml:check-inline-table-conflict keys inline-table-registry)
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
          ;; Check if this array table conflicts with an inline table
          (toml:check-inline-table-conflict keys inline-table-registry)
          ;; Check if this array table conflicts with an existing single table
          (when (member keys table-history)
            (signal 'toml-array-table-error (list (point))))
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
        ;; Check all intermediate prefixes for scalar conflicts
        (let ((prefix nil))
          (dolist (seg (butlast current-table))
            (setq prefix (append prefix (list seg)))
            (when (or (member prefix scalar-key-registry)
                      (let ((elm (toml:assoc prefix hashes)))
                        (and elm (not (toml:alistp (cdr elm))))))
              (signal 'toml-redefine-key-error (list (point))))))
        ;; Check the full path
        (let ((elm (toml:assoc current-table hashes)))
          (when (and elm (not (toml:alistp (cdr elm))))
            (signal 'toml-redefine-key-error (list (point))))))

      ;; Read key-value pair
      (let ((key-segments (toml:read-key)))
        (when key-segments
          (let* ((dotted-table (butlast key-segments))
                 (leaf-key (car (last key-segments)))
                 (effective-table (append current-table dotted-table)))
            ;; Check for key redefinition
            (let ((full-path (if current-array-table
                                 nil
                               (append effective-table (list leaf-key)))))
              (when (and full-path (toml:assoc full-path hashes))
                (signal 'toml-redefine-key-error (list (point))))
              ;; Check that intermediate dotted-table paths are not non-table values
              (when (and full-path dotted-table)
                (let ((prefix current-table))
                  (dolist (seg dotted-table)
                    (setq prefix (append prefix (list seg)))
                    (when (or (member prefix scalar-key-registry)
                              (let ((existing (toml:assoc prefix hashes)))
                                (and existing (not (toml:alistp (cdr existing))))))
                      (signal 'toml-redefine-key-error (list (point)))))))
              ;; Check inline table immutability
              (when full-path
                (toml:check-inline-table-conflict full-path inline-table-registry))

              (toml:ensure-value-on-same-line)
              (setq current-value (toml:read-value))

              ;; Register scalar key paths (non-alist values)
              ;; Note: (toml:alistp nil) returns t since nil is an empty list,
              ;; but false/nil is a scalar value, so we check explicitly.
              ;; :toml-empty-table is an empty table, not a scalar.
              (when (and full-path
                         (not (eq current-value :toml-empty-table))
                         (or (not (toml:alistp current-value))
                             (null current-value)))
                (push full-path scalar-key-registry)))

            ;; Register inline table paths
            (when (and (not current-array-table) current-value
                       (or (toml:alistp current-value) (eq current-value :toml-empty-table)))
              (let ((base-path (append effective-table (list leaf-key))))
                (if (eq current-value :toml-empty-table)
                    (push base-path inline-table-registry)
                  (dolist (path (toml:collect-inline-table-paths base-path current-value))
                    (push path inline-table-registry)))))

            ;; Register dotted-key-defined implicit tables in table-history
            ;; to prevent [table] headers from reopening them (TOML v1.0.0)
            (when (and (not current-array-table) dotted-table)
              (let ((prefix current-table))
                (dolist (seg dotted-table)
                  (setq prefix (append prefix (list seg)))
                  ;; Dotted keys must not add to explicitly defined tables
                  (when (member prefix explicit-table-registry)
                    (signal 'toml-redefine-table-error (list (point))))
                  (unless (member prefix table-history)
                    (push prefix table-history)))))

            ;; Add to appropriate structure
            (if current-array-table
                ;; Add to array table element
                (setq hashes (toml:add-to-array-table current-array-table
                                                      (append current-array-sub-keys dotted-table)
                                                      leaf-key
                                                      current-value
                                                      hashes
                                                      array-table-registry))
              ;; Add to regular table
              (setq hashes (toml:make-table-hashes effective-table
                                                   leaf-key
                                                   current-value
                                                   hashes)))

            ;; After key-value pair, ensure no extra tokens on the same line
            (skip-chars-forward " \t")
            (unless (or (eobp) (toml:end-of-line-p) (eq (toml:get-char-at-point) ?#))
              (signal 'toml-key-error (list (point)))))))

      (toml:seek-readable-point))
    hashes))

(defun toml:--validate-string-characters ()
  "Check for raw-byte characters in the current multibyte buffer.
Signals `toml-encoding-error' if problematic characters are found.
This provides best-effort validation for `toml:read-from-string'."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((ch (char-after)))
        (when (and (>= ch #x3FFF80) (<= ch #x3FFFFF))
          (signal 'toml-encoding-error
                  (list (format "Raw byte character at pos %d" (point))))))
      (forward-char 1))))

(defun toml:read-from-string (string)
  "Read the TOML object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (toml:--validate-string-characters)
    (toml:--normalize-newlines)
    (goto-char (point-min))
    (toml:read)))

(defun toml:--expect-continuation-bytes (n)
  "Advance past a multi-byte UTF-8 sequence expecting N continuation bytes.
Point should be on the leading byte.  Signals `toml-encoding-error'
if any continuation byte is missing or invalid (not 10xxxxxx)."
  (let ((start (point)))
    (forward-char 1)
    (dotimes (_ n)
      (if (eobp)
          (signal 'toml-encoding-error
                  (list (format "Truncated UTF-8 sequence starting at pos %d" start)))
        (let ((b (char-after)))
          (unless (and (>= b #x80) (<= b #xBF))
            (signal 'toml-encoding-error
                    (list (format "Invalid continuation byte 0x%02X at pos %d"
                                  b (point)))))
          (forward-char 1))))))

(defun toml:--validate-utf8-bytes ()
  "Validate that the current unibyte buffer contains well-formed UTF-8.
Signals `toml-encoding-error' if invalid bytes are found.
Must be called in a unibyte buffer (e.g., read with `binary').

Why manual byte validation instead of Emacs built-in functions:
- `detect-coding-string': An encoding guesser, not a validator.
  It cannot reliably reject invalid UTF-8 (e.g., surrogate 0xED 0xA0 0x80
  may be guessed as `utf-8'; results vary by environment).
- `decode-coding-string'/`decode-coding-region' with `utf-8': Does not
  signal errors on invalid input.  Instead, it silently converts bad bytes
  to raw-byte characters (#x3FFF80-#x3FFFFF).  This catches most cases,
  but surrogate codepoints (U+D800-U+DFFF) are decoded as normal
  characters (e.g., 0xED 0xA0 0x80 → U+D800), not raw-bytes.
- `check-coding-systems-region': Returns nil (= OK) for all invalid
  UTF-8 patterns tested, providing no validation."
  (goto-char (point-min))
  ;; Check for UTF-16 BOM
  (when (>= (buffer-size) 2)
    (let ((b0 (char-after (point-min)))
          (b1 (char-after (1+ (point-min)))))
      (when (or (and (= b0 #xFE) (= b1 #xFF))
                (and (= b0 #xFF) (= b1 #xFE)))
        (signal 'toml-encoding-error (list "UTF-16 BOM detected")))))
  ;; Skip UTF-8 BOM at file start only
  (when (and (>= (buffer-size) 3)
             (= (char-after (point-min)) #xEF)
             (= (char-after (+ (point-min) 1)) #xBB)
             (= (char-after (+ (point-min) 2)) #xBF))
    (goto-char (+ (point-min) 3)))
  ;; Walk through bytes validating UTF-8 sequences
  (while (not (eobp))
    (let ((byte (char-after)))
      (cond
       ;; ASCII: 0x00-0x7F
       ((<= byte #x7F)
        (forward-char 1))
       ;; 2-byte: 0xC2-0xDF
       ((and (>= byte #xC2) (<= byte #xDF))
        (toml:--expect-continuation-bytes 1))
       ;; 3-byte: 0xE0-0xEF
       ((and (>= byte #xE0) (<= byte #xEF))
        (let ((b1 (char-after (1+ (point)))))
          ;; 0xE0: reject overlong (second byte must be >= 0xA0)
          (when (and (= byte #xE0) b1 (< b1 #xA0))
            (signal 'toml-encoding-error
                    (list (format "Overlong 3-byte sequence at pos %d" (point)))))
          ;; 0xED: reject surrogates U+D800-U+DFFF (second byte must be < 0xA0)
          (when (and (= byte #xED) b1 (>= b1 #xA0))
            (signal 'toml-encoding-error
                    (list (format "Surrogate codepoint at pos %d" (point))))))
        (toml:--expect-continuation-bytes 2))
       ;; 4-byte: 0xF0-0xF4
       ((and (>= byte #xF0) (<= byte #xF4))
        (let ((b1 (char-after (1+ (point)))))
          ;; 0xF0: reject overlong (second byte must be >= 0x90)
          (when (and (= byte #xF0) b1 (< b1 #x90))
            (signal 'toml-encoding-error
                    (list (format "Overlong 4-byte sequence at pos %d" (point)))))
          ;; 0xF4: reject > U+10FFFF (second byte must be <= 0x8F)
          (when (and (= byte #xF4) b1 (> b1 #x8F))
            (signal 'toml-encoding-error
                    (list (format "Codepoint > U+10FFFF at pos %d" (point))))))
        (toml:--expect-continuation-bytes 3))
       ;; Invalid leading byte (0x80-0xBF, 0xC0-0xC1, 0xF5-0xFF)
       (t
        (signal 'toml-encoding-error
                (list (format "Invalid UTF-8 byte 0x%02X at pos %d"
                              byte (point)))))))))

(defun toml:read-from-file (file)
  "Read the TOML object contained in FILE and return it."
  (with-temp-buffer
    ;; Phase 1: Read as raw bytes and validate UTF-8
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary))
      (insert-file-contents file))
    (toml:--validate-utf8-bytes)
    ;; Phase 2: Convert to multibyte and parse
    (decode-coding-region (point-min) (point-max) 'utf-8-with-signature)
    (set-buffer-multibyte t)
    (toml:--normalize-newlines)
    (goto-char (point-min))
    (toml:read)))

(defun toml:--normalize-newlines ()
  "Replace CRLF with LF in the current buffer.
The TOML spec allows parsers to normalize newlines to whatever
makes sense for their platform (see toml-lang/toml#281).
On Unix-like systems, normalizing CRLF to LF at the entry point
keeps the parser simple while correctly rejecting bare CR."
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" t t)))

(provide 'toml)

;;; toml.el ends here
