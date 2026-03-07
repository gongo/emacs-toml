;;; toml.el --- TOML (Tom's Obvious, Minimal Language) parser -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-toml
;; Keywords: toml parser
;; Version: 0.5.0

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
\\([0-5][0-9]\\):\
\\([0-5][0-9]\\)\
\\(?:\\.\\([0-9]+\\)\\)?\
\\(Z\\|[+-][0-9]\\{2\\}:[0-9]\\{2\\}\\)"
  "Regular expression for RFC 3339 datetime with timezone and fractional seconds.")

(defconst toml->regexp-local-datetime
  "\
\\([0-9]\\{4\\}\\)-\
\\(0[1-9]\\|1[0-2]\\)-\
\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)[Tt ]\
\\([0-1][0-9]\\|2[0-3]\\):\
\\([0-5][0-9]\\):\
\\([0-5][0-9]\\)\
\\(?:\\.\\([0-9]+\\)\\)?"
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
\\([0-5][0-9]\\):\
\\([0-5][0-9]\\)\
\\(?:\\.\\([0-9]+\\)\\)?"
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

(put 'toml-inline-table-error 'error-message "Bad inline table")
(put 'toml-inline-table-error 'error-conditions
     '(toml-inline-table-error toml-error error))

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
         (mapped (assq char toml->escape-sequence-alist)))
    (cond
     (mapped (char-to-string (cdr mapped)))
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
                     (not (eq char ?\n))
                     (not (eq char ?\r)))
            (signal 'toml-string-error (list (point))))
          ;; Regular escape sequence or regular character
          (if (eq char ?\\)
              (push (toml:read-escaped-char) characters)
            (push (toml:read-char) characters))))))
    ;; Skip the closing """
    (forward-char 3)
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
                   (not (eq char ?\n))
                   (not (eq char ?\r)))
          (signal 'toml-string-error (list (point)))))
      (push (toml:read-char) characters))
    ;; Skip the closing '''
    (forward-char 3)
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
        (second   (string-to-number (match-string-no-properties 6)))
        (fraction (match-string-no-properties 7))  ; optional
        (timezone (match-string-no-properties 8))) ; Z or +HH:MM or -HH:MM
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
        (second   (string-to-number (match-string-no-properties 6)))
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
        (second   (string-to-number (match-string-no-properties 3)))
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
  (let (elements char-after-read)
    (while (not (char-equal (toml:get-char-at-point) ?}))
      (let* ((key-segments (toml:read-key))
             (value (toml:read-value)))
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
              (toml:seek-readable-point)
              ;; TODO: Trailing comma in inline tables is allowed from TOML v1.1.0.
              ;; Until then, it should be rejected.
              (when (char-equal (toml:get-char-at-point) ?})
                (signal 'toml-inline-table-error (list (point)))))
          (signal 'toml-inline-table-error (list (point))))))
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
       ((char-equal (toml:get-char-at-point) ?\") (toml:read-string))
       ((char-equal (toml:get-char-at-point) ?\') (toml:read-literal-string))
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
       ((char-equal (toml:get-char-at-point) ?\") (toml:read-string))
       ((char-equal (toml:get-char-at-point) ?\') (toml:read-literal-string))
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
                    (setcdr existing (vconcat current-val (vector nil)))
                    hashes)
                ;; Conflict: trying to redefine non-array as array table
                (signal 'toml-array-table-error (list (point)))))
          ;; Create new array with one empty element
          (cons (cons key (vector nil)) hashes)))
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
                (setcdr existing-child (vconcat child-array (vector nil)))))
          ;; Create new nested array
          (let ((new-elem (cons (cons child-key (vector nil)) current-elem)))
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
                    (let ((existing (toml:assoc prefix hashes)))
                      (when (and existing (not (toml:alistp (cdr existing))))
                        (signal 'toml-redefine-key-error (list (point)))))))))

            (setq current-value (toml:read-value))

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
                                                   hashes))))))

      (toml:seek-readable-point))
    hashes))

(defun toml:read-from-string (string)
  "Read the TOML object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (toml:--normalize-newlines)
    (goto-char (point-min))
    (toml:read)))

(defun toml:read-from-file (file)
  "Read the TOML object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
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
