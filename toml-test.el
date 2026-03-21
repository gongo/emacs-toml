(require 'ert)
(require 'toml)

(defmacro toml-test:buffer-setup (string &rest body)
  `(with-temp-buffer
     (insert ,string)
     (goto-char (point-min))
     ,@body))

(ert-deftest toml-test:seek-beginning-of-next-line ()
  (toml-test:buffer-setup
   "\12345\n67890\nabcde"

   (toml:seek-beginning-of-next-line)
   (should (eq (char-after (point)) ?6))

   (forward-char 3) ;; "8" on the second line
   (toml:seek-beginning-of-next-line)
   (should (eq (toml:get-char-at-point) ?a))))

(ert-deftest toml-test:seek-readable-point ()
  (toml-test:buffer-setup
   "\

  # comment line
  # comment line 2 # 3
aiueo"

   (toml:seek-readable-point)
   (should (eq (char-after (point)) ?a))))

(ert-deftest toml-test:seek-non-whitespace ()
  (toml-test:buffer-setup
   "  hello\n\t \tworld"

   (toml:seek-non-whitespace)
   (should (eq (char-after (point)) ?h))

   (toml:seek-beginning-of-next-line)
   (toml:seek-non-whitespace)
   (should (eq (toml:get-char-at-point) ?w))))

(ert-deftest toml-test:read-char ()
  (toml-test:buffer-setup
   "aiueo"
   (should (equal "a" (toml:read-char)))
   (should (equal ?i (toml:get-char-at-point)))

   (should (equal "i" (toml:read-char)))
   (should (equal ?u (toml:get-char-at-point)))))

(ert-deftest toml-test:read-char-with-char-p ()
  (toml-test:buffer-setup
   "aiueo"
   (should (equal ?a (toml:read-char t)))
   (should (equal ?i (toml:get-char-at-point)))

   (should (equal ?i (toml:read-char t)))
   (should (equal ?u (toml:get-char-at-point)))))

(ert-deftest toml-test:read-escaped-char ()
  (dolist (test '(("\\b" . "\b")
                  ("\\t" . "\t")
                  ("\\n" . "\n")
                  ("\\f" . "\f")
                  ("\\r" . "\r")
                  ("\\e" . "\e")
                  ("\\\"" . "\"")
                  ("\\\\" . "\\")
                  ("\\x00" . "\x00")
                  ("\\x61" . "a")
                  ("\\xFF" . "ÿ")
                  ("\\x1B" . "\e")
                  ("\\u0041" . "A")
                  ("\\u1234" . "\u1234")
                  ("\\uD799" . "\uD799")
                  ("\\uE000" . "\uE000")
                  ("\\U0001F600" . "😀")
                  ("\\U00000041" . "A")
                  ("\\U0010FFFF" . "\U0010FFFF")
                  ))
    (toml-test:buffer-setup
     (car test)
     (should (equal (cdr test) (toml:read-escaped-char)))
     (should (toml:end-of-line-p)))))

(ert-deftest toml-test-error:read-escaped-char ()
  (dolist (char '(" "
		  " \\b"
		  "a"
		  "\\a"
		  "\\c"
		  "\\/"
		  "\\uABC!"
		  "\\u____"
                  "\\UABCDEFG!"
		  "\\U________"
		  "\\U0001F60"
		  "\\xGG"
		  "\\x1"
		  "\\uD800"
		  "\\uDFFF"
		  "\\U0000D800"
		  "\\U0000DFFF"
		  "\\U00110000"
		  ))
    (toml-test:buffer-setup
     char
     (should-error (toml:read-escaped-char) :type 'toml-string-escape-error))))

(ert-deftest toml-test:read-string ()
  (toml-test:buffer-setup
   "\"GitHub Cofounder & CEO\\nLikes tater tots and beer.\""
   (should (equal "GitHub Cofounder & CEO\nLikes tater tots and beer." (toml:read-string)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-string ()
  (dolist (str '("aiueo"       ;; Not start with '"'
                 "\"hogehoge"  ;; Not end with '"'
                 " \"aiueo\""  ;; Not start with '"'
                 ))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-string) :type 'toml-string-error))))

(ert-deftest toml-test:read-string-control-char ()
  (dolist (test '((#x09 . "\t") ;; U+0009 (TAB)
                  (#x20 . " ") ;; U+0020 (SPACE)
		  (#x7E . "~") ;; U+007E (TILDE)
                  ))
    (toml-test:buffer-setup
     (format "\"%c\"" (car test))
     (let ((result (toml:read-string)))
       (should (equal (cdr test) result))))

    (toml-test:buffer-setup
     (format "'%c'" (car test))
     (let ((result (toml:read-literal-string)))
       (should (equal (cdr test) result))))
    ))

(ert-deftest toml-test-error:read-string-control-char ()
  "Test that control characters are rejected in basic strings."
  (dolist (char '(#x00 ;; U+0000 (null)
                  #x1F ;; U+001F (last C0 control char)
		  #x7F ;; U+007F (DEL)
                  ))
    (toml-test:buffer-setup
     (format "\"%c\"" char)
     (should-error (toml:read-string) :type 'toml-string-error))))

(ert-deftest toml-test-error:read-literal-string-control-char ()
  "Test that control characters are rejected in literal strings."
  (dolist (char '(#x00 ;; U+0000 (null)
                  #x1F ;; U+001F (last C0 control char)
		  #x7F ;; U+007F (DEL)
                  ))
    (toml-test:buffer-setup
     (format "'%c'" char)
     (should-error (toml:read-literal-string) :type 'toml-string-error))))

(ert-deftest toml-test-error:read-comment ()
  "Test that control characters are rejected in comments."
  (dolist (char '(#x00 ;; U+0000 (null)
                  #x1F ;; U+001F (last C0 control char)
                  #x7F ;; U+007F (DEL)
                  ))
    (toml-test:buffer-setup
     (format "# comment %c here" char)
     (should-error (toml:seek-readable-point) :type 'toml-comment-error)))
  ;; Tab (U+0009) is allowed in comments
  (toml-test:buffer-setup
   (format "# comment %c here" #x09)
   (should-not (condition-case nil
                   (progn (toml:seek-readable-point) nil)
                 (toml-comment-error t)))))

(ert-deftest toml-test:read-boolean ()
  (toml-test:buffer-setup
   "true"
   (should (equal t (toml:read-boolean)))
   (should (toml:end-of-line-p)))

  (toml-test:buffer-setup
   "false"
   (should (null (toml:read-boolean)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-boolean ()
  (toml-test:buffer-setup
   "truu"
   (should-error (toml:read-boolean) :type 'toml-boolean-error))

  (toml-test:buffer-setup
   " false"
   (should-error (toml:read-boolean) :type 'toml-boolean-error))

  ;; mixed-case
  (toml-test:buffer-setup
   "falsE"
   (should-error (toml:read-boolean) :type 'toml-boolean-error))

  (toml-test:buffer-setup
   "trUe"
   (should-error (toml:read-boolean) :type 'toml-boolean-error)))

(ert-deftest toml-test:validate-date ()
  (should (null (toml:validate-date 2000 1 31)))
  (should (null (toml:validate-date 2000 2 29)))  ; leap year
  (should (null (toml:validate-date 1900 2 28)))  ; not leap year
  (should (null (toml:validate-date 2000 4 30)))
  (should-error (toml:validate-date 2000 2 30) :type 'toml-datetime-error)  ; leap year but 30
  (should-error (toml:validate-date 1900 2 29) :type 'toml-datetime-error)  ; not leap year
  (should-error (toml:validate-date 2100 2 29) :type 'toml-datetime-error)  ; not leap year
  (should-error (toml:validate-date 2000 4 31) :type 'toml-datetime-error)) ; April has 30

(ert-deftest toml-test:read-datetime ()
  (toml-test:buffer-setup
   "1979-05-27T07:32:00Z"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (null (cdr (assoc 'fraction dt))))
     (should (equal "Z" (cdr (assoc 'timezone dt)))))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test:read-datetime-with-timezone ()
  (toml-test:buffer-setup
   "1979-05-27T00:32:00-07:00"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (equal 0 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (null (cdr (assoc 'fraction dt))))
     (should (equal "-07:00" (cdr (assoc 'timezone dt))))))

  (toml-test:buffer-setup
   "1979-05-27T00:32:00+05:30"
   (let ((dt (toml:read-datetime)))
     (should (equal "+05:30" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:read-datetime-with-fractional-seconds ()
  (toml-test:buffer-setup
   "1979-05-27T00:32:00.999999-07:00"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (equal 0 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (floatp (cdr (assoc 'fraction dt))))
     (should (equal 0.999999 (cdr (assoc 'fraction dt))))
     (should (equal "-07:00" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:read-datetime-space-delimiter ()
  (toml-test:buffer-setup
   "1979-05-27 07:32:00Z"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (equal "Z" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:read-datetime-lowercase-t ()
  (toml-test:buffer-setup
   "1979-05-27t07:32:00Z"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (equal "Z" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:read-datetime-space-with-timezone ()
  (toml-test:buffer-setup
   "1979-05-27 00:32:00-07:00"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 0 (cdr (assoc 'hour dt))))
     (should (equal "-07:00" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:read-local-datetime ()
  (toml-test:buffer-setup
   "1979-05-27T07:32:00"
   (let ((dt (toml:read-local-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (null (cdr (assoc 'fraction dt))))
     (should (null (assoc 'timezone dt))))))

(ert-deftest toml-test:read-local-datetime-with-fraction ()
  (toml-test:buffer-setup
   "1979-05-27T07:32:00.999"
   (let ((dt (toml:read-local-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (floatp (cdr (assoc 'fraction dt))))
     (should (equal 0.999 (cdr (assoc 'fraction dt))))
     (should (null (assoc 'timezone dt))))))

(ert-deftest toml-test:read-local-datetime-space-delimiter ()
  (toml-test:buffer-setup
   "1979-05-27 07:32:00"
   (let ((dt (toml:read-local-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (null (assoc 'timezone dt))))))

(ert-deftest toml-test:read-local-date ()
  (toml-test:buffer-setup
   "1979-05-27"
   (let ((dt (toml:read-local-date)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5 (cdr (assoc 'month dt))))
     (should (equal 27 (cdr (assoc 'day dt))))
     (should (null (assoc 'hour dt)))
     (should (null (assoc 'timezone dt)))))

  (toml-test:buffer-setup
   "2000-01-01"
   (let ((dt (toml:read-local-date)))
     (should (equal 2000 (cdr (assoc 'year dt))))
     (should (equal 1 (cdr (assoc 'month dt))))
     (should (equal 1 (cdr (assoc 'day dt)))))))

(ert-deftest toml-test:read-local-time ()
  (toml-test:buffer-setup
   "07:32:00"
   (let ((dt (toml:read-local-time)))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (equal 32 (cdr (assoc 'minute dt))))
     (should (equal 0 (cdr (assoc 'second dt))))
     (should (null (cdr (assoc 'fraction dt))))
     (should (null (assoc 'year dt))))))

(ert-deftest toml-test:read-local-time-end-of-day ()
  (toml-test:buffer-setup
   "23:59:59"
   (let ((dt (toml:read-local-time)))
     (should (equal 23 (cdr (assoc 'hour dt))))
     (should (equal 59 (cdr (assoc 'minute dt))))
     (should (equal 59 (cdr (assoc 'second dt)))))))

(ert-deftest toml-test:read-local-time-with-fraction ()
  (toml-test:buffer-setup
   "07:32:00.999"
   (let ((dt (toml:read-local-time)))
     (should (equal 7 (cdr (assoc 'hour dt))))
     (should (floatp (cdr (assoc 'fraction dt))))
     (should (equal 0.999 (cdr (assoc 'fraction dt)))))))

(ert-deftest toml-test-error:read-datetime ()
  (dolist (str '("1979-35-27T07:32:00Z" " 1979-05-27T07:32:00Z"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-datetime) :type 'toml-datetime-error))))

(ert-deftest toml-test-error:datetime-offset-overflow-hour ()
  (toml-test:buffer-setup
   "1985-06-18T17:04:07+25:00"
   (should-error (toml:read-datetime) :type 'toml-datetime-error)))

(ert-deftest toml-test-error:datetime-offset-overflow-minute ()
  (toml-test:buffer-setup
   "1985-06-18T17:04:07+12:60"
   (should-error (toml:read-datetime) :type 'toml-datetime-error)))

(ert-deftest toml-test:read-numeric ()
  (toml-test:buffer-setup
   "1"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1 numeric))
     (should (integerp numeric))
     (should (wholenump numeric))))

  (toml-test:buffer-setup
   "42"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 42 numeric))
     (should (integerp numeric))
     (should (wholenump numeric))))

  (toml-test:buffer-setup
   "-17"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal -17 numeric))
     (should (integerp numeric))
     (should (not (wholenump numeric)))))

  (toml-test:buffer-setup
   "3.1415"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 3.1415 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "-0.01"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal -0.01 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "+42"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 42 numeric))
     (should (integerp numeric))))

  (toml-test:buffer-setup
   "+3.14"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 3.14 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "+0"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 0 numeric))
     (should (integerp numeric))))

  (toml-test:buffer-setup
   "5e+22"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 5e+22 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "1e6"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1e6 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "-2E-2"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal -0.02 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "6.626e-34"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 6.626e-34 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "1E6"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1e6 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "+1.5e10"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1.5e10 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "1_000"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1000 numeric))
     (should (integerp numeric))))

  (toml-test:buffer-setup
   "5_349_221"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 5349221 numeric))
     (should (integerp numeric))))

  (toml-test:buffer-setup
   "9_224_617.445_991_228_313"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 9224617.445991228313 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "1_0e1_0"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1e11 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "5e06" ;; Leading zeroes in exponent parts of floats
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 5e6 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "1e0022"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 1e22 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "3.14e007"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal 3.14e7 numeric))
     (should (floatp numeric))))

  (toml-test:buffer-setup
   "-1e-001"
   (let ((numeric (toml:read-numeric)))
     (should (toml:end-of-line-p))
     (should (equal -0.1 numeric))
     (should (floatp numeric)))))

(ert-deftest toml-test:read-numeric-hex ()
  (dolist (test '(("0xDEADBEEF" . 3735928559)
                  ("0xdeadbeef" . 3735928559)
                  ("0xdead_beef" . 3735928559)
                  ("0x0" . 0)
                  ("0xff" . 255)
                  ("0x00ff" . 255)))
    (toml-test:buffer-setup
     (car test)
     (let ((result (toml:read-numeric)))
       (should (equal (cdr test) result))
       (should (integerp result))))))

(ert-deftest toml-test:read-numeric-oct ()
  (dolist (test '(("0o755" . 493)
                  ("0o01234567" . 342391)
                  ("0o0" . 0)))
    (toml-test:buffer-setup
     (car test)
     (let ((result (toml:read-numeric)))
       (should (equal (cdr test) result))
       (should (integerp result))))))

(ert-deftest toml-test:read-numeric-bin ()
  (dolist (test '(("0b11010110" . 214)
                  ("0b0" . 0)
                  ("0b1111_0000" . 240)))
    (toml-test:buffer-setup
     (car test)
     (let ((result (toml:read-numeric)))
       (should (equal (cdr test) result))
       (should (integerp result))))))

(ert-deftest toml-test-error:read-numeric-prefixed ()
  ;; Uppercase prefixes are invalid
  (dolist (str '("0X1" "0O7" "0B1"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error)))
  ;; Underscore after prefix is invalid (caught by regexp not matching)
  (dolist (str '("0x_deadbeef" "0o_7" "0b_1"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error))))

(ert-deftest toml-test-error:read-numeric ()
  (dolist (str '("" "- 1.1" " 1.1" ".1" "1.1.1" "1.1.1.1"
                 "1e" "1e+" "1e-" "1E+" "1." "1.e5"
                 "_1000" "1000_" "1__000" "1._0" "1.0_e5"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error))))

(ert-deftest toml-test:read-numeric-leading-zero ()
  ;; Zero and zero-prefixed floats/exponents are valid
  (dolist (test '(("0" . 0) ("0.0" . 0.0) ("0e1" . 0.0)))
    (toml-test:buffer-setup
     (car test)
     (should (equal (cdr test) (toml:read-numeric))))))

(ert-deftest toml-test-error:read-numeric-leading-zero ()
  ;; Integer leading zeros
  (dolist (str '("01" "00" "0_0" "-01" "+01" "+0_1"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error)))
  ;; Float leading zeros
  (dolist (str '("03.14" "-03.14" "+03.14"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error))))

(ert-deftest toml-test:read-key ()
  (toml-test:buffer-setup
   "a = 3"
   (should (equal '("a") (toml:read-key)))
   (should (eq ?3 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "biueo = true"
   (should (equal '("biueo") (toml:read-key)))
   (should (eq ?t (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "connection_max = 5000"
   (should (equal '("connection_max") (toml:read-key)))
   (should (eq ?5 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "connection-max = name"
   (should (equal '("connection-max") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "server12 = name"
   (should (equal '("server12") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "'key' = name"
   (should (equal '("key") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "\"key\" = name"
   (should (equal '("key") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "'' = name"
   (should (equal '("") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "\"\" = name"
   (should (equal '("") (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "1biueo = true"
   (should (equal '("1biueo") (toml:read-key)))
   (should (eq ?t (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "connection_ = 5000"
   (should (equal '("connection_") (toml:read-key)))
   (should (eq ?5 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "1234 = 42"
   (should (equal '("1234") (toml:read-key)))
   (should (eq ?4 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "_ = 1"
   (should (equal '("_") (toml:read-key)))
   (should (eq ?1 (toml:get-char-at-point))))

  ;; Dotted keys
  (toml-test:buffer-setup
   "physical.color = \"orange\""
   (should (equal '("physical" "color") (toml:read-key)))
   (should (eq ?\" (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "a.b.c = 1"
   (should (equal '("a" "b" "c") (toml:read-key)))
   (should (eq ?1 (toml:get-char-at-point))))

  ;; Dotted key with spaces around dot
  (toml-test:buffer-setup
   "fruit . color = \"red\""
   (should (equal '("fruit" "color") (toml:read-key)))
   (should (eq ?\" (toml:get-char-at-point))))

  ;; Dotted key with quoted segments
  (toml-test:buffer-setup
   "site.\"google.com\" = true"
   (should (equal '("site" "google.com") (toml:read-key)))
   (should (eq ?t (toml:get-char-at-point))))
  )


(ert-deftest toml-test-error:read-key ()
  ;; no key - "=" doesn't match any key pattern, so read-key-segment signals error
  (toml-test:buffer-setup
   " = 3"
   (should-error (toml:read-key) :type 'toml-key-error))

  ;; key only
  (toml-test:buffer-setup
   "key"
   (should-error (toml:read-key) :type 'toml-key-error))

  ;; multiline not allowed
  (toml-test:buffer-setup
   "\"key\nagain\" = name"
   (should-error (toml:read-key) :type 'toml-key-error))
  )


(ert-deftest toml-test:read-array ()
  (toml-test:buffer-setup
   "[]"
   (should (equal [] (toml:read-array))))

  (toml-test:buffer-setup
   "[1, 2, 3]"
   (should (equal [1 2 3] (toml:read-array))))

  (toml-test:buffer-setup
   "[ [1, 2], [3, 4, 5] ]"
   (should (equal [[1 2] [3 4 5]] (toml:read-array))))

  (toml-test:buffer-setup
   "[
      1,
      2,
    ]"
   (should (equal [1 2] (toml:read-array))))

  (toml-test:buffer-setup
   "[1, \"a\", 2.0]"
   (should (equal [1 "a" 2.0] (toml:read-array))))

  (toml-test:buffer-setup
   "[1, 0.2]"
   (should (equal [1 0.2] (toml:read-array))))
  )

(ert-deftest toml-test-error:read-array ()
  (toml-test:buffer-setup
   "[1 2 3]"
   (should-error (toml:read-array) :type 'toml-array-error))

  )

(ert-deftest toml-test:read-array-multiline-after-bracket ()
  (let ((parsed (toml:read-from-string "a = [\n]")))
    (should (equal (cdr (assoc "a" parsed)) [])))
  (let ((parsed (toml:read-from-string "a = { a = [\n]}")))
    (should (equal (cdr (assoc "a" (cdr (assoc "a" parsed)))) []))))

(ert-deftest toml-test:read-inline-table ()
  (toml-test:buffer-setup
   "{}"
   (should (eq :toml-empty-table (toml:read-inline-table))))

  (toml-test:buffer-setup
   "{ name = \"Tom\" }"
   (let ((result (toml:read-inline-table)))
     (should (equal "Tom" (cdr (assoc "name" result))))))

  (toml-test:buffer-setup
   "{ first = \"Tom\", last = \"Preston-Werner\" }"
   (let ((result (toml:read-inline-table)))
     (should (equal "Tom" (cdr (assoc "first" result))))
     (should (equal "Preston-Werner" (cdr (assoc "last" result))))))

  (toml-test:buffer-setup
   "{ x = 1, y = 2 }"
   (let ((result (toml:read-inline-table)))
     (should (equal 1 (cdr (assoc "x" result))))
     (should (equal 2 (cdr (assoc "y" result))))))

  (toml-test:buffer-setup
   "{ inner = { key = \"value\" } }"
   (let ((result (toml:read-inline-table)))
     (should (equal "value" (cdr (assoc "key" (cdr (assoc "inner" result))))))))

  (toml-test:buffer-setup
   "{ ports = [8001, 8002] }"
   (let ((result (toml:read-inline-table)))
     (should (equal [8001 8002] (cdr (assoc "ports" result)))))))

(ert-deftest toml-test-error:read-inline-table ()
  ;; Missing comma between key-value pairs
  (toml-test:buffer-setup
   "{ a = 1 b = 2 }"
   (should-error (toml:read-inline-table) :type 'toml-inline-table-error))

  ;; Trailing comma is allowed
  (toml-test:buffer-setup
   "{ a = 1, b = 2, }"
   (let ((result (toml:read-inline-table)))
     (should (equal 1 (cdr (assoc "a" result))))
     (should (equal 2 (cdr (assoc "b" result)))))))

(ert-deftest toml-test:read-inline-table-multiline ()
  ;; Empty table with newline
  (toml-test:buffer-setup
   "{\n}"
   (should (eq :toml-empty-table (toml:read-inline-table))))

  ;; Single pair with newlines
  (toml-test:buffer-setup
   "{\n  name = \"Tom\"\n}"
   (let ((result (toml:read-inline-table)))
     (should (equal "Tom" (cdr (assoc "name" result))))))

  ;; Multiple pairs with newlines
  (toml-test:buffer-setup
   "{\n  a = 1,\n  b = 2\n}"
   (let ((result (toml:read-inline-table)))
     (should (equal 1 (cdr (assoc "a" result))))
     (should (equal 2 (cdr (assoc "b" result))))))

  ;; Multiple pairs with newlines and trailing comma
  (toml-test:buffer-setup
   "{\n  a = 1,\n  b = 2,\n}"
   (let ((result (toml:read-inline-table)))
     (should (equal 1 (cdr (assoc "a" result))))
     (should (equal 2 (cdr (assoc "b" result))))))

  ;; With comment
  (toml-test:buffer-setup
   "{\n  # comment\n  a = 1\n}"
   (let ((result (toml:read-inline-table)))
     (should (equal 1 (cdr (assoc "a" result)))))))

(ert-deftest toml-test:read-empty-inline-table-semantics ()
  "Test that empty inline tables are distinct from boolean false."
  ;; Top-level empty inline table
  (toml-test:buffer-setup
   "empty = {}"
   (let* ((parsed (toml:read))
          (val (cdr (assoc "empty" parsed))))
     (should (eq :toml-empty-table val))))

  ;; Nested inline table containing empty
  (toml-test:buffer-setup
   "outer = {inner = {}}"
   (let* ((parsed (toml:read))
          (outer (cdr (assoc "outer" parsed)))
          (inner (cdr (assoc "inner" outer))))
     (should (eq :toml-empty-table inner))))

  ;; Array containing empty inline table
  (toml-test:buffer-setup
   "arr = [{}, {a = 1}]"
   (let* ((parsed (toml:read))
          (arr (cdr (assoc "arr" parsed))))
     (should (eq :toml-empty-table (aref arr 0)))
     (should (equal 1 (cdr (assoc "a" (aref arr 1))))))))

(ert-deftest toml-test:read-from-string-inline-table-multiline ()
  (let ((result (toml:read-from-string "\
name = {
  first = \"Tom\",
  last = \"Preston-Werner\",
}")))
    (should (equal "Tom" (cdr (assoc "first" (cdr (assoc "name" result))))))
    (should (equal "Preston-Werner" (cdr (assoc "last" (cdr (assoc "name" result))))))))

(ert-deftest toml-test:read-table ()
  (toml-test:buffer-setup
   "[aiueo]"
   (should (equal '(:type single :keys ("aiueo")) (toml:read-table))))

  (toml-test:buffer-setup
   "[ai-ueo]"
   (should (equal '(:type single :keys ("ai-ueo")) (toml:read-table))))

  (toml-test:buffer-setup
   "[servers]
    [servers.alpha]

       key = value"
   (should (equal '(:type single :keys ("servers" "alpha")) (toml:read-table)))
   (should (eq ?k (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "[servers]
    [servers.alpha]
    [client]"
   (should (equal '(:type single :keys ("client")) (toml:read-table))))

  (toml-test:buffer-setup
   "[[aiueo]]"
   (should (equal '(:type array :keys ("aiueo")) (toml:read-table))))

  (toml-test:buffer-setup
   "[[ai-ueo]]"
   (should (equal '(:type array :keys ("ai-ueo")) (toml:read-table))))

  (toml-test:buffer-setup
   "[1234]"
   (should (equal '(:type single :keys ("1234")) (toml:read-table))))

  (toml-test:buffer-setup
   "[foo.bar_]"
   (should (equal '(:type single :keys ("foo" "bar_")) (toml:read-table))))

  (toml-test:buffer-setup
   "[[servers]]
    [[servers.alpha]]

       key = value"
   (should (equal '(:type array :keys ("servers")) (toml:read-table)))
   (should (eq ?\[ (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "[[servers]]
    [[servers.alpha]]
    [[client]]"
   (should (equal '(:type array :keys ("servers")) (toml:read-table))))

  ;; Quoted keys in table headers
  (toml-test:buffer-setup
   "[dog.\"tater.man\"]"
   (should (equal '(:type single :keys ("dog" "tater.man")) (toml:read-table))))

  (toml-test:buffer-setup
   "[dog.'tater.man']"
   (should (equal '(:type single :keys ("dog" "tater.man")) (toml:read-table))))

  (toml-test:buffer-setup
   "[\"dog\".tater]"
   (should (equal '(:type single :keys ("dog" "tater")) (toml:read-table))))

  ;; Spaces around dots and brackets with quoted keys
  (toml-test:buffer-setup
   "[ dog . \"tater.man\" ]"
   (should (equal '(:type single :keys ("dog" "tater.man")) (toml:read-table))))

  ;; Array of tables with quoted keys
  (toml-test:buffer-setup
   "[[dog.\"tater.man\"]]"
   (should (equal '(:type array :keys ("dog" "tater.man")) (toml:read-table))))
  )

(ert-deftest toml-test:read-table-quoted-key-integration ()
  ;; Integration: quoted key in table header with key/value pairs
  (toml-test:buffer-setup
   "
[dog.\"tater.man\"]
type.name = \"pug\""
   (should (equal
            '(("dog" . (("tater.man" . (("type" . (("name" . "pug"))))))))
            (toml:read))))

  ;; Literal string key in table header
  (toml-test:buffer-setup
   "
[dog.'tater.man']
type = \"pug\""
   (should (equal
            '(("dog" . (("tater.man" . (("type" . "pug"))))))
            (toml:read))))

  ;; Quoted key at first segment
  (toml-test:buffer-setup
   "
[\"dog\".tater]
type = \"pug\""
   (should (equal
            '(("dog" . (("tater" . (("type" . "pug"))))))
            (toml:read))))

  ;; Multiple tables with quoted keys
  (toml-test:buffer-setup
   "
[a.\"b.c\"]
x = 1

[a.\"d.e\"]
y = 2"
   (should (equal
            '(("a"
	       ("d.e" ("y" . 2))
               ("b.c" ("x" . 1))))
            (toml:read))))

  ;; Array of tables with quoted keys
  (toml-test:buffer-setup
   "
[[fruit.\"physical.prop\"]]
color = \"red\"

[[fruit.\"physical.prop\"]]
color = \"green\""
   (should (equal
            '(("fruit" . (("physical.prop" . [(("color" . "red"))
                                              (("color" . "green"))]))))
            (toml:read)))))

(ert-deftest toml-test-error:read-table ()
  (toml-test:buffer-setup
   "[]"
   (should-error (toml:read-table) :type 'toml-table-error))

  ;; end with period "."
  (toml-test:buffer-setup
   "[foo.bar.]"
   (should-error (toml:read-table) :type 'toml-table-error))

  ;; consecutive dots
  (toml-test:buffer-setup
   "[foo..bar]"
   (should-error (toml:read-table) :type 'toml-table-error)))

(ert-deftest toml-test:make-table-hashes ()
  (let (hash)
    (setq hash (toml:make-table-hashes '("servers" "alpha") "ip" "192.0.2.1" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))

    (setq hash (toml:make-table-hashes '("servers" "alpha") "dc" "eqdc10" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))

    (setq hash (toml:make-table-hashes '("servers" "beta") "dc" "eqdc10" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "beta"  "dc") hash)))

    (setq hash (toml:make-table-hashes '("client") "ip" "192.0.2.123" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "beta"  "dc") hash)))
    (should (equal '("ip" . "192.0.2.123") (toml:assoc '("client" "ip") hash)))

    ;; update
    (setq hash (toml:make-table-hashes '("servers" "alpha") "ip" "192.0.2.233" hash))
    (should (equal '("ip" . "192.0.2.233") (toml:assoc '("servers" "alpha" "ip") hash)))))

(ert-deftest toml-test:parse()
  (toml-test:buffer-setup
   "\
\[a.b\]
c = 1
g = { bar = 4711, baz = \"foo\" }

\[a\]
d = 2
e = []
f = { foo = 2342 }
"
   (let ((parsed (toml:read)))
     (should (equal '("c" . 1) (toml:assoc '("a" "b" "c") parsed)))
     (should (equal '("d" . 2) (toml:assoc '("a" "d") parsed)))
     (should (equal '("e" . []) (toml:assoc '("a" "e") parsed)))
     (should (equal '("f" . (("foo" . 2342))) (toml:assoc '("a" "f") parsed)))
     (should (equal '("g" ("bar" . 4711) ("baz" . "foo")) (toml:assoc '("a" "b" "g") parsed)))
     ))

  (toml-test:buffer-setup
   "\
# This is a TOML document. Boom.

title = \"TOML Example\"

\[owner\]
name = \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

\[database\]
server = \"192.168.1.1\"
ports = \[ 8001, 8001, 8002 \]
connection_max = 5000
enabled = true

\[servers\]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  \[servers.alpha\]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  \[servers.beta\]
  ip = \"10.0.0.2\"
  dc = \"eqdc10\"

\[clients\]
data = \[ \[\"gamma\", \"delta\"\], \[1, 2\] \]

# Line breaks are OK when inside arrays
hosts = \[
  \"alpha\",
  \"omega\"
\]"
   (let ((parsed (toml:read)))
     (should (toml:assoc '("servers" "beta" "dc") parsed))
     (should (toml:assoc '("clients" "data") parsed))
     (should (toml:assoc '("database" "ports") parsed))
     )))


(ert-deftest toml-test-error:parse ()
  (toml-test:buffer-setup
   "\
\[a\]
b = 1

\[a\]
c = 2"
   (should-error (toml:read) :type 'toml-redefine-table-error))

  (toml-test:buffer-setup
   "\
\[a\]
b = 1

\[a.b\]
c = 2"
   (should-error (toml:read) :type 'toml-redefine-key-error))
  )

(ert-deftest toml-test:read-literal-string ()
  (toml-test:buffer-setup
   "'Literal string with no escapes.'"
   (should (equal "Literal string with no escapes." (toml:read-literal-string)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test:read-empty-literal-string ()
  (toml-test:buffer-setup
   "''"
   (should (equal "" (toml:read-literal-string)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-literal-string ()
  (dolist (case '(("'unterminated string"   . toml-string-error)
                  ("'''"                    . toml-string-error)))
    (let* ((input (concat "x = " (car case)))
           (expected-error (cdr case)))
      (toml-test:buffer-setup
       input
       (if expected-error
           (should-error (toml:read) :type expected-error)
         (should (toml:read)))))))

;; Array of Tables tests

(ert-deftest toml-test:read-array-of-tables-basic ()
  "Test basic array of tables parsing."
  (toml-test:buffer-setup
   "[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]
name = \"Nail\"
sku = 284758393"
   (let ((parsed (toml:read)))
     ;; products should be a vector
     (should (vectorp (cdr (assoc "products" parsed))))
     ;; Should have 2 elements
     (should (= 2 (length (cdr (assoc "products" parsed)))))
     ;; First element
     (let ((first (aref (cdr (assoc "products" parsed)) 0)))
       (should (equal "Hammer" (cdr (assoc "name" first))))
       (should (equal 738594937 (cdr (assoc "sku" first)))))
     ;; Second element
     (let ((second (aref (cdr (assoc "products" parsed)) 1)))
       (should (equal "Nail" (cdr (assoc "name" second))))
       (should (equal 284758393 (cdr (assoc "sku" second))))))))

(ert-deftest toml-test:read-empty-array-of-tables ()
  "Test array of tables with empty elements."
  (toml-test:buffer-setup
   "[[products]]
name = \"Hammer\"

[[products]]

[[products]]
name = \"Nail\""
   (let ((parsed (toml:read)))
     (let ((products (cdr (assoc "products" parsed))))
       ;; Should have 3 elements
       (should (= 3 (length products)))
       ;; First element has name
       (should (equal "Hammer" (cdr (assoc "name" (aref products 0)))))
       ;; Second element is empty table (:toml-empty-table sentinel)
       (should (eq :toml-empty-table (aref products 1)))
       ;; Third element has name
       (should (equal "Nail" (cdr (assoc "name" (aref products 2)))))))))

(ert-deftest toml-test:read-array-of-tables-with-subtable ()
  "Test array of tables with sub-tables."
  (toml-test:buffer-setup
   "[[fruits]]
name = \"apple\"

[fruits.physical]
color = \"red\"
shape = \"round\"

[[fruits]]
name = \"banana\"

[fruits.physical]
color = \"yellow\"
shape = \"long\""
   (let ((parsed (toml:read)))
     (let ((fruits (cdr (assoc "fruits" parsed))))
       ;; Should have 2 elements
       (should (= 2 (length fruits)))
       ;; First element
       (let ((first (aref fruits 0)))
         (should (equal "apple" (cdr (assoc "name" first))))
         (let ((physical (cdr (assoc "physical" first))))
           (should (equal "red" (cdr (assoc "color" physical))))
           (should (equal "round" (cdr (assoc "shape" physical))))))
       ;; Second element
       (let ((second (aref fruits 1)))
         (should (equal "banana" (cdr (assoc "name" second))))
         (let ((physical (cdr (assoc "physical" second))))
           (should (equal "yellow" (cdr (assoc "color" physical))))
           (should (equal "long" (cdr (assoc "shape" physical))))))))))

(ert-deftest toml-test:read-nested-array-of-tables ()
  "Test nested array of tables."
  (toml-test:buffer-setup
   "[[fruits]]
name = \"apple\"

[[fruits.varieties]]
name = \"red delicious\"

[[fruits.varieties]]
name = \"granny smith\"

[[fruits]]
name = \"banana\"

[[fruits.varieties]]
name = \"plantain\""
   (let ((parsed (toml:read)))
     (let ((fruits (cdr (assoc "fruits" parsed))))
       ;; Should have 2 fruit elements
       (should (= 2 (length fruits)))
       ;; First fruit (apple)
       (let ((apple (aref fruits 0)))
         (should (equal "apple" (cdr (assoc "name" apple))))
         (let ((varieties (cdr (assoc "varieties" apple))))
           (should (vectorp varieties))
           (should (= 2 (length varieties)))
           (should (equal "red delicious" (cdr (assoc "name" (aref varieties 0)))))
           (should (equal "granny smith" (cdr (assoc "name" (aref varieties 1)))))))
       ;; Second fruit (banana)
       (let ((banana (aref fruits 1)))
         (should (equal "banana" (cdr (assoc "name" banana))))
         (let ((varieties (cdr (assoc "varieties" banana))))
           (should (vectorp varieties))
           (should (= 1 (length varieties)))
           (should (equal "plantain" (cdr (assoc "name" (aref varieties 0)))))))))))

;; Error tests for Array of Tables

(ert-deftest toml-test-error:array-table-static-array ()
  "Test that appending to a statically defined array is an error."
  (toml-test:buffer-setup
   "fruits = []

[[fruits]]
name = \"apple\""
   (should-error (toml:read) :type 'toml-array-table-error)))

(ert-deftest toml-test-error:array-table-conflicts-with-table ()
  "Test that array table conflicts with existing table."
  (toml-test:buffer-setup
   "[fruits]
name = \"apple\"

[[fruits]]
name = \"banana\""
   (should-error (toml:read) :type 'toml-array-table-error)))

(ert-deftest toml-test-error:table-conflicts-with-array-table ()
  "Test that table conflicts with existing array table."
  (toml-test:buffer-setup
   "[[fruits]]
name = \"apple\"

[fruits]
name = \"banana\""
   (should-error (toml:read) :type 'toml-array-table-error)))

(ert-deftest toml-test:read-deeply-nested-array-of-tables ()
  "Test deeply nested array of tables with sub-tables."
  (toml-test:buffer-setup
   "[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]
name = \"Nail\"
sku = 284758393

[products.colors]
primary = \"silver\"

[[products]]
name = \"Screwdriver\""
   (let ((parsed (toml:read)))
     (let ((products (cdr (assoc "products" parsed))))
       ;; Should have 3 elements
       (should (= 3 (length products)))
       ;; First element
       (should (equal "Hammer" (cdr (assoc "name" (aref products 0)))))
       ;; Second element with colors sub-table
       (let ((second (aref products 1)))
         (should (equal "Nail" (cdr (assoc "name" second))))
         (let ((colors (cdr (assoc "colors" second))))
           (should (equal "silver" (cdr (assoc "primary" colors))))))
       ;; Third element
       (should (equal "Screwdriver" (cdr (assoc "name" (aref products 2)))))))))

(ert-deftest toml-test:read-multiple-nested-array-tables ()
  "Test multiple nested array tables at different levels."
  (toml-test:buffer-setup
   "[[servers]]
name = \"server1\"

[[servers.interfaces]]
ip = \"10.0.0.1\"

[[servers.interfaces]]
ip = \"10.0.0.2\"

[[servers]]
name = \"server2\"

[[servers.interfaces]]
ip = \"192.168.1.1\""
   (let ((parsed (toml:read)))
     (let ((servers (cdr (assoc "servers" parsed))))
       ;; Should have 2 server elements
       (should (= 2 (length servers)))
       ;; First server
       (let ((server1 (aref servers 0)))
         (should (equal "server1" (cdr (assoc "name" server1))))
         (let ((interfaces (cdr (assoc "interfaces" server1))))
           (should (vectorp interfaces))
           (should (= 2 (length interfaces)))
           (should (equal "10.0.0.1" (cdr (assoc "ip" (aref interfaces 0)))))
           (should (equal "10.0.0.2" (cdr (assoc "ip" (aref interfaces 1)))))))
       ;; Second server
       (let ((server2 (aref servers 1)))
         (should (equal "server2" (cdr (assoc "name" server2))))
         (let ((interfaces (cdr (assoc "interfaces" server2))))
           (should (vectorp interfaces))
           (should (= 1 (length interfaces)))
           (should (equal "192.168.1.1" (cdr (assoc "ip" (aref interfaces 0)))))))))))

;; Dotted key tests

(ert-deftest toml-test:parse-dotted-key-basic ()
  "Test basic dotted key parsing."
  (let ((parsed (toml:read-from-string "
physical.color = \"orange\"
physical.shape = \"round\"")))
    (should (equal "orange" (cdr (assoc "color" (cdr (assoc "physical" parsed))))))
    (should (equal "round" (cdr (assoc "shape" (cdr (assoc "physical" parsed))))))))

(ert-deftest toml-test:parse-dotted-key-with-quoted-segment ()
  "Test dotted key with quoted key segments."
  (let ((parsed (toml:read-from-string "site.\"google.com\" = true")))
    (should (equal t (cdr (assoc "google.com" (cdr (assoc "site" parsed))))))))

(ert-deftest toml-test:parse-dotted-key-under-table ()
  "Test dotted key under a table header."
  (let ((parsed (toml:read-from-string "
[fruit]
physical.color = \"orange\"
physical.shape = \"round\"")))
    (let ((fruit (cdr (assoc "fruit" parsed))))
      (should (equal "orange" (cdr (assoc "color" (cdr (assoc "physical" fruit))))))
      (should (equal "round" (cdr (assoc "shape" (cdr (assoc "physical" fruit)))))))))

(ert-deftest toml-test:parse-dotted-key-with-spaces ()
  "Test dotted key with spaces around dot."
  (let ((parsed (toml:read-from-string "fruit . color = \"red\"")))
    (should (equal "red" (cdr (assoc "color" (cdr (assoc "fruit" parsed))))))))

(ert-deftest toml-test:parse-dotted-key-in-inline-table ()
  "Test dotted key inside inline table."
  (let ((parsed (toml:read-from-string "point = { x.y = 1, x.z = 2 }")))
    (let ((point (cdr (assoc "point" parsed))))
      (should (equal 1 (cdr (assoc "y" (cdr (assoc "x" point))))))
      (should (equal 2 (cdr (assoc "z" (cdr (assoc "x" point)))))))))

(ert-deftest toml-test:parse-deep-dotted-key-in-inline-table ()
  "Test deeply nested dotted keys with shared prefix in inline table."
  (let ((parsed (toml:read-from-string "t = { a.b.c = 1, a.b.d = 2 }")))
    (let* ((tbl (cdr (assoc "t" parsed)))
           (b (cdr (assoc "b" (cdr (assoc "a" tbl))))))
      (should (equal 1 (cdr (assoc "c" b))))
      (should (equal 2 (cdr (assoc "d" b)))))))

(ert-deftest toml-test:parse-dotted-key-in-array-of-tables ()
  "Test dotted key inside array of tables."
  (let ((parsed (toml:read-from-string "
[[fruits]]
physical.color = \"red\"

[[fruits]]
physical.color = \"yellow\"")))
    (let ((fruits (cdr (assoc "fruits" parsed))))
      (should (vectorp fruits))
      (should (= 2 (length fruits)))
      (should (equal "red" (cdr (assoc "color" (cdr (assoc "physical" (aref fruits 0)))))))
      (should (equal "yellow" (cdr (assoc "color" (cdr (assoc "physical" (aref fruits 1))))))))))

(ert-deftest toml-test-error:parse-dotted-key-redefine ()
  "Test that redefining a dotted key is an error."
  (should-error (toml:read-from-string "
a.b = 1
a.b = 2")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:parse-dotted-key-extend-non-table ()
  "Test that extending a non-table value with dotted key is an error."
  (should-error (toml:read-from-string "
a.b = 1
a.b.c = 2")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test:read-array-table-with-nested-subtable ()
  "Test array of tables with deeply nested sub-tables."
  (toml-test:buffer-setup
   "[[books]]
title = \"TOML Guide\"

[books.author]
name = \"John\"

[books.author.address]
city = \"Tokyo\"

[[books]]
title = \"Emacs Manual\"

[books.author]
name = \"Jane\""
   (let ((parsed (toml:read)))
     (let ((books (cdr (assoc "books" parsed))))
       ;; Should have 2 book elements
       (should (= 2 (length books)))
       ;; First book with nested sub-tables
       (let ((book1 (aref books 0)))
         (should (equal "TOML Guide" (cdr (assoc "title" book1))))
         (let ((author (cdr (assoc "author" book1))))
           (should (equal "John" (cdr (assoc "name" author))))
           (let ((address (cdr (assoc "address" author))))
             (should (equal "Tokyo" (cdr (assoc "city" address)))))))
       ;; Second book
       (let ((book2 (aref books 1)))
         (should (equal "Emacs Manual" (cdr (assoc "title" book2))))
         (let ((author (cdr (assoc "author" book2))))
           (should (equal "Jane" (cdr (assoc "name" author))))))))))

(ert-deftest toml-test:read-multiline-basic-string ()
  (toml-test:buffer-setup
   "\"\"\"Hello
World\"\"\""
   (should (equal "Hello\nWorld" (toml:read-string))))

  (toml-test:buffer-setup
   "\"\"\"
One
Two\"\"\""
   (should (equal "One\nTwo" (toml:read-string)))))

(ert-deftest toml-test:read-multiline-basic-string-line-continuation ()
  (toml-test:buffer-setup
   "\"\"\"The quick brown \\
    fox jumps over \\
    the lazy dog.\"\"\""
   (should (equal "The quick brown fox jumps over the lazy dog." (toml:read-string))))

  (toml-test:buffer-setup
   "\"\"\"The quick \\


    fox\"\"\""
   (should (equal "The quick fox" (toml:read-string)))))

(ert-deftest toml-test:read-multiline-basic-string-escapes ()
  (toml-test:buffer-setup
   "\"\"\"Line1\\nLine2\"\"\""
   (should (equal "Line1\nLine2" (toml:read-string))))

  (toml-test:buffer-setup
   "\"\"\"Tab\\there\"\"\""
   (should (equal "Tab\there" (toml:read-string)))))

(ert-deftest toml-test:read-multiline-basic-string-quotes ()
  (toml-test:buffer-setup
   "\"\"\"Hello \"\"World\"\"\""
   (should (equal "Hello \"\"World" (toml:read-string))))

  (toml-test:buffer-setup
   "\"\"\"Say \"Hello\" to me\"\"\""
   (should (equal "Say \"Hello\" to me" (toml:read-string)))))

(ert-deftest toml-test:read-multiline-basic-string-extra-quotes ()
  ;; 4-quote closing: """" = content + closing """
  (toml-test:buffer-setup
   "\"\"\"\"This,\" she said, \"is just a pointless statement.\"\"\"\""
   (should (equal "\"This,\" she said, \"is just a pointless statement.\"" (toml:read-string))))

  ;; 5-quote closing: """"" = content + closing """
  (toml-test:buffer-setup
   "\"\"\"\"\"I'm a five-quote string\"\"\"\"\""
   (should (equal "\"\"I'm a five-quote string\"\"" (toml:read-string))))

  ;; 7 quotes: """ + " + """ = single quote content
  (toml-test:buffer-setup
   (make-string 7 ?\")
   (should (equal "\"" (toml:read-string))))

  ;; 8 quotes: """ + "" + """ = two quote content
  (toml-test:buffer-setup
   (make-string 8 ?\")
   (should (equal "\"\"" (toml:read-string)))))

(ert-deftest toml-test-error:read-multiline-basic-string-too-many-quotes ()
  ;; 9 quotes: """ + 6 = error
  (toml-test:buffer-setup
   (make-string 9 ?\")
   (should-error (toml:read-string) :type 'toml-string-error)))

(ert-deftest toml-test:read-multiline-basic-string-empty ()
  (toml-test:buffer-setup
   "\"\"\"\"\"\""
   (should (equal "" (toml:read-string)))))

(ert-deftest toml-test-error:read-multiline-basic-string-unterminated ()
  (toml-test:buffer-setup
   "\"\"\"unterminated"
   (should-error (toml:read-string) :type 'toml-string-error)))

(ert-deftest toml-test-error:read-multiline-basic-string-bare-cr ()
  "Bare CR (not part of CRLF) must be rejected in multiline basic strings."
  (toml-test:buffer-setup
   "\"\"\"\rhello\"\"\""
   (should-error (toml:read-string) :type 'toml-string-error)))

(ert-deftest toml-test:read-multiline-literal-string ()
  (toml-test:buffer-setup
   "'''Hello
World'''"
   (should (equal "Hello\nWorld" (toml:read-literal-string))))

  (toml-test:buffer-setup
   "'''
Text here'''"
   (should (equal "Text here" (toml:read-literal-string)))))

(ert-deftest toml-test:read-multiline-literal-string-no-escapes ()
  (toml-test:buffer-setup
   "'''I [dw]on't need \\d{2} apples'''"
   (should (equal "I [dw]on't need \\d{2} apples" (toml:read-literal-string))))

  (toml-test:buffer-setup
   "'''line1\\
line2'''"
   (should (equal "line1\\\nline2" (toml:read-literal-string)))))

(ert-deftest toml-test:read-multiline-literal-string-quotes ()
  (toml-test:buffer-setup
   "'''Hello ''World'''"
   (should (equal "Hello ''World" (toml:read-literal-string)))))

(ert-deftest toml-test:read-multiline-literal-string-extra-quotes ()
  ;; 4-quote closing: '''' = content + closing '''
  (toml-test:buffer-setup
   "''''That,' she said, 'is still pointless.''''"
   (should (equal "'That,' she said, 'is still pointless.'" (toml:read-literal-string))))

  ;; 5-quote closing: ''''' = content + closing '''
  (toml-test:buffer-setup
   "'''''I have apostrophes'''''"
   (should (equal "''I have apostrophes''" (toml:read-literal-string))))

  ;; 7 quotes: ''' + ' + ''' = single quote content
  (toml-test:buffer-setup
   "'''''''"
   (should (equal "'" (toml:read-literal-string))))

  ;; 8 quotes: ''' + '' + ''' = two quote content
  (toml-test:buffer-setup
   "''''''''"
   (should (equal "''" (toml:read-literal-string)))))

(ert-deftest toml-test-error:read-multiline-literal-string-too-many-quotes ()
  ;; 9 quotes: ''' + 6 = error
  (toml-test:buffer-setup
   "'''''''''"
   (should-error (toml:read-literal-string) :type 'toml-string-error)))

(ert-deftest toml-test:read-multiline-literal-string-empty ()
  (toml-test:buffer-setup
   "''''''"
   (should (equal "" (toml:read-literal-string)))))

(ert-deftest toml-test-error:read-multiline-literal-string-unterminated ()
  (toml-test:buffer-setup
   "'''unterminated"
   (should-error (toml:read-literal-string) :type 'toml-string-error)))

(ert-deftest toml-test-error:read-multiline-literal-string-bare-cr ()
  "Bare CR (not part of CRLF) must be rejected in multiline literal strings."
  (toml-test:buffer-setup
   "'''\rhello'''"
   (should-error (toml:read-literal-string) :type 'toml-string-error)))

(ert-deftest toml-test:parse-crlf ()
  "Test that CRLF line endings are handled correctly."
  (let ((parsed (toml:read-from-string "key1 = \"value1\"\r\nkey2 = \"value2\"")))
    (should (equal "value1" (cdr (assoc "key1" parsed))))
    (should (equal "value2" (cdr (assoc "key2" parsed)))))

  (let ((parsed (toml:read-from-string "[section]\r\nkey = 42\r\n")))
    (should (equal 42 (cdr (assoc "key" (cdr (assoc "section" parsed))))))))

(ert-deftest toml-test:read-multiline-basic-string-crlf ()
  "Test multiline basic strings with CRLF line endings."
  (let ((parsed (toml:read-from-string "key = \"\"\"\r\nHello\r\nWorld\"\"\"")))
    (should (equal "Hello\nWorld" (cdr (assoc "key" parsed)))))

  (let ((parsed (toml:read-from-string "key = \"\"\"The quick brown \\\r\n    fox\"\"\"")))
    (should (equal "The quick brown fox" (cdr (assoc "key" parsed))))))

(ert-deftest toml-test:read-multiline-literal-string-crlf ()
  "Test multiline literal strings with CRLF line endings."
  (let ((parsed (toml:read-from-string "key = '''\r\nHello\r\nWorld'''")))
    (should (equal "Hello\nWorld" (cdr (assoc "key" parsed))))))

;; Special float tests (inf, nan)

(ert-deftest toml-test:read-special-float ()
  "Test parsing of special float values inf and nan."
  (toml-test:buffer-setup
   "inf"
   (should (equal 1.0e+INF (toml:read-special-float))))

  (toml-test:buffer-setup
   "nan"
   (let ((result (toml:read-special-float)))
     (should (isnan result)))))

(ert-deftest toml-test:parse-special-float-values ()
  "Test special float values in full TOML parsing."
  (let ((parsed (toml:read-from-string "
val_inf = inf
val_pinf = +inf
val_ninf = -inf
val_nan = nan
val_pnan = +nan
val_nnan = -nan")))
    (should (equal 1.0e+INF (cdr (assoc "val_inf" parsed))))
    (should (equal 1.0e+INF (cdr (assoc "val_pinf" parsed))))
    (should (equal -1.0e+INF (cdr (assoc "val_ninf" parsed))))
    (should (isnan (cdr (assoc "val_nan" parsed))))
    (should (isnan (cdr (assoc "val_pnan" parsed))))
    (should (isnan (cdr (assoc "val_nnan" parsed))))))

(ert-deftest toml-test-error:read-special-float ()
  "Test that invalid special float values are rejected."
  (dolist (input '("Inf" "INF" "Infinity" "infinity" "+Infinity"
                   "NaN" "NAN"))
    (toml-test:buffer-setup
     (concat "x = " input)
     (should-error (toml:read) :type 'toml-error))))

;; Hex/Oct/Bin integration tests

(ert-deftest toml-test:parse-prefixed-integers ()
  "Test hex, oct, bin integers in full TOML parsing."
  (let ((parsed (toml:read-from-string "
hex1 = 0xDEADBEEF
hex2 = 0xdead_beef
oct1 = 0o755
bin1 = 0b11010110")))
    (should (equal 3735928559 (cdr (assoc "hex1" parsed))))
    (should (equal 3735928559 (cdr (assoc "hex2" parsed))))
    (should (equal 493 (cdr (assoc "oct1" parsed))))
    (should (equal 214 (cdr (assoc "bin1" parsed))))))

(ert-deftest toml-test-error:parse-signed-prefixed-integers ()
  "Test that signed prefixed integers are rejected."
  (should-error (toml:read-from-string "x = +0xff")
                :type 'toml-error)
  (should-error (toml:read-from-string "x = -0xff")
                :type 'toml-error))

(ert-deftest toml-test:parse-multiline-strings ()
  (toml-test:buffer-setup
   "str1 = \"\"\"
Roses are red
Violets are blue\"\"\"

str2 = '''
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
'''"
   (let ((parsed (toml:read)))
     (should (equal "Roses are red\nViolets are blue"
                    (cdr (assoc "str1" parsed))))
     (should (equal "The first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n"
                    (cdr (assoc "str2" parsed))))))

  (toml-test:buffer-setup
   "[section]
multiline = \"\"\"
This is a
multiline value
\"\"\"

regex = '''\\d{4}-\\d{2}-\\d{2}'''
"
   (let ((parsed (toml:read)))
     (should (equal "This is a\nmultiline value\n"
                    (cdr (assoc "multiline" (cdr (assoc "section" parsed))))))
     (should (equal "\\d{4}-\\d{2}-\\d{2}"
                    (cdr (assoc "regex" (cdr (assoc "section" parsed)))))))))

(ert-deftest toml-test-error:inline-table-immutable-dotted-key ()
  "Dotted key extending an inline table should error."
  (should-error (toml:read-from-string "type = { name = \"Nail\" }\ntype.edible = false")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-reopen-table ()
  "Reopening an inline table with [table] should error."
  (should-error (toml:read-from-string "type = { name = \"Nail\" }\n[type]\nedible = false")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-nested-dotted ()
  "Extending a nested inline table with dotted key should error."
  (should-error (toml:read-from-string "t = { inner = { x = 1 } }\nt.inner.y = 2")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-nested-reopen ()
  "Reopening a nested inline table with [table] should error."
  (should-error (toml:read-from-string "t = { inner = { x = 1 } }\n[t.inner]\ny = 2")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-under-table ()
  "Extending an inline table under [table] with dotted key should error."
  (should-error (toml:read-from-string "[product]\ntype = { name = \"Nail\" }\ntype.edible = false")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-under-table-reopen ()
  "Reopening an inline table under [table] with [table] header should error."
  (should-error (toml:read-from-string "[product]\ntype = { name = \"Nail\" }\n[product.type]\nedible = false")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test-error:inline-table-immutable-dotted-key-intermediate ()
  "Dotted key creating intermediate table in inline table is also immutable."
  (should-error (toml:read-from-string "t = { a.b = 1 }\nt.a.c = 2")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test:inline-table-independent-ok ()
  "Independent inline tables should not conflict."
  (let ((parsed (toml:read-from-string "a = { x = 1 }\nb = { y = 2 }")))
    (should (equal 1 (cdr (assoc "x" (cdr (assoc "a" parsed))))))
    (should (equal 2 (cdr (assoc "y" (cdr (assoc "b" parsed))))))))

(ert-deftest toml-test:inline-table-different-path-ok ()
  "Dotted key and inline table at different paths should not conflict."
  (let ((parsed (toml:read-from-string "a.b = 1\nc = { d = 2 }")))
    (should (equal 1 (cdr (assoc "b" (cdr (assoc "a" parsed))))))
    (should (equal 2 (cdr (assoc "d" (cdr (assoc "c" parsed))))))))

(ert-deftest toml-test-error:dotted-key-implicit-table-reopen-root ()
  "Reopening implicit table defined by root dotted key should error."
  (should-error (toml:read-from-string "fruit.apple.color = \"red\"\n[fruit.apple]")
                :type 'toml-redefine-table-error))

(ert-deftest toml-test-error:dotted-key-implicit-table-reopen-under-table ()
  "Reopening implicit table defined by dotted key under [table] should error."
  (should-error (toml:read-from-string "[fruit]\napple.color = \"red\"\n[fruit.apple]")
                :type 'toml-redefine-table-error))

(ert-deftest toml-test-error:dotted-key-implicit-table-reopen-nested ()
  "Reopening nested implicit table defined by dotted key should error."
  (should-error (toml:read-from-string "[fruit]\napple.taste.sweet = true\n[fruit.apple.taste]")
                :type 'toml-redefine-table-error))

(ert-deftest toml-test-error:dotted-key-implicit-table-reopen-self ()
  "Reopening root implicit table itself should error."
  (should-error (toml:read-from-string "a.b.c = 1\n[a]")
                :type 'toml-redefine-table-error))

(ert-deftest toml-test:dotted-key-new-subtable-ok ()
  "New subtable under dotted-key-defined table should be allowed."
  (let ((parsed (toml:read-from-string "fruit.apple.color = \"red\"\n[fruit.apple.texture]\nsmooth = true")))
    (should (equal "red" (cdr (assoc "color" (cdr (assoc "apple" (cdr (assoc "fruit" parsed))))))))
    (should (equal t (cdr (assoc "smooth" (cdr (assoc "texture" (cdr (assoc "apple" (cdr (assoc "fruit" parsed))))))))))))

(ert-deftest toml-test:dotted-key-multiple-same-prefix-ok ()
  "Multiple dotted keys with same implicit prefix should be allowed."
  (let ((parsed (toml:read-from-string "a.b = 1\na.c = 2")))
    (should (equal 1 (cdr (assoc "b" (cdr (assoc "a" parsed))))))
    (should (equal 2 (cdr (assoc "c" (cdr (assoc "a" parsed))))))))

(ert-deftest toml-test:dotted-key-under-table-same-prefix-ok ()
  "Multiple dotted keys with same prefix under [table] should be allowed."
  (let ((parsed (toml:read-from-string "[a]\nb.c = 1\nb.d = 2")))
    (should (equal 1 (cdr (assoc "c" (cdr (assoc "b" (cdr (assoc "a" parsed))))))))
    (should (equal 2 (cdr (assoc "d" (cdr (assoc "b" (cdr (assoc "a" parsed))))))))))

(ert-deftest toml-test:datetime-seconds-optional-Z ()
  "Offset datetime with seconds omitted and Z timezone."
  (toml-test:buffer-setup
   "1979-05-27T07:32Z"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5    (cdr (assoc 'month dt))))
     (should (equal 27   (cdr (assoc 'day dt))))
     (should (equal 7    (cdr (assoc 'hour dt))))
     (should (equal 32   (cdr (assoc 'minute dt))))
     (should (equal 0    (cdr (assoc 'second dt))))
     (should (equal nil  (cdr (assoc 'fraction dt))))
     (should (equal "Z"  (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:datetime-seconds-optional-positive-offset ()
  "Offset datetime with seconds omitted and positive timezone offset."
  (toml-test:buffer-setup
   "1979-05-27T07:32+05:30"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979     (cdr (assoc 'year dt))))
     (should (equal 5        (cdr (assoc 'month dt))))
     (should (equal 27       (cdr (assoc 'day dt))))
     (should (equal 7        (cdr (assoc 'hour dt))))
     (should (equal 32       (cdr (assoc 'minute dt))))
     (should (equal 0        (cdr (assoc 'second dt))))
     (should (equal nil      (cdr (assoc 'fraction dt))))
     (should (equal "+05:30" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:datetime-seconds-optional-negative-offset ()
  "Offset datetime with seconds omitted and negative timezone offset."
  (toml-test:buffer-setup
   "1979-05-27T07:32-07:00"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979     (cdr (assoc 'year dt))))
     (should (equal 5        (cdr (assoc 'month dt))))
     (should (equal 27       (cdr (assoc 'day dt))))
     (should (equal 7        (cdr (assoc 'hour dt))))
     (should (equal 32       (cdr (assoc 'minute dt))))
     (should (equal 0        (cdr (assoc 'second dt))))
     (should (equal nil      (cdr (assoc 'fraction dt))))
     (should (equal "-07:00" (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:datetime-seconds-optional-space-separator ()
  "Offset datetime with seconds omitted and space separator."
  (toml-test:buffer-setup
   "1979-05-27 07:32Z"
   (let ((dt (toml:read-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5    (cdr (assoc 'month dt))))
     (should (equal 27   (cdr (assoc 'day dt))))
     (should (equal 7    (cdr (assoc 'hour dt))))
     (should (equal 32   (cdr (assoc 'minute dt))))
     (should (equal 0    (cdr (assoc 'second dt))))
     (should (equal nil  (cdr (assoc 'fraction dt))))
     (should (equal "Z"  (cdr (assoc 'timezone dt)))))))

(ert-deftest toml-test:local-datetime-seconds-optional ()
  "Local datetime with seconds omitted."
  (toml-test:buffer-setup
   "1979-05-27T07:32"
   (let ((dt (toml:read-local-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5    (cdr (assoc 'month dt))))
     (should (equal 27   (cdr (assoc 'day dt))))
     (should (equal 7    (cdr (assoc 'hour dt))))
     (should (equal 32   (cdr (assoc 'minute dt))))
     (should (equal 0    (cdr (assoc 'second dt))))
     (should (equal nil  (cdr (assoc 'fraction dt)))))))

(ert-deftest toml-test:local-datetime-seconds-optional-space ()
  "Local datetime with seconds omitted and space separator."
  (toml-test:buffer-setup
   "1979-05-27 07:32"
   (let ((dt (toml:read-local-datetime)))
     (should (equal 1979 (cdr (assoc 'year dt))))
     (should (equal 5    (cdr (assoc 'month dt))))
     (should (equal 27   (cdr (assoc 'day dt))))
     (should (equal 7    (cdr (assoc 'hour dt))))
     (should (equal 32   (cdr (assoc 'minute dt))))
     (should (equal 0    (cdr (assoc 'second dt))))
     (should (equal nil  (cdr (assoc 'fraction dt)))))))

(ert-deftest toml-test:local-time-seconds-optional ()
  "Local time with seconds omitted."
  (toml-test:buffer-setup
   "07:32"
   (let ((dt (toml:read-local-time)))
     (should (equal 7   (cdr (assoc 'hour dt))))
     (should (equal 32  (cdr (assoc 'minute dt))))
     (should (equal 0   (cdr (assoc 'second dt))))
     (should (equal nil (cdr (assoc 'fraction dt)))))))

(ert-deftest toml-test:datetime-seconds-optional-integration ()
  "Integration test: parse seconds-optional datetime values via toml:read-from-string."
  (let ((parsed (toml:read-from-string "\
odt1 = 1979-05-27T07:32Z
odt2 = 1979-05-27T07:32-07:00
ldt1 = 1979-05-27T07:32
lt1 = 07:32
")))
    (let ((odt1 (cdr (assoc "odt1" parsed))))
      (should (equal 0   (cdr (assoc 'second odt1))))
      (should (equal "Z" (cdr (assoc 'timezone odt1)))))
    (let ((odt2 (cdr (assoc "odt2" parsed))))
      (should (equal 0        (cdr (assoc 'second odt2))))
      (should (equal "-07:00" (cdr (assoc 'timezone odt2)))))
    (let ((ldt1 (cdr (assoc "ldt1" parsed))))
      (should (equal 0 (cdr (assoc 'second ldt1)))))
    (let ((lt1 (cdr (assoc "lt1" parsed))))
      (should (equal 0 (cdr (assoc 'second lt1)))))))

(ert-deftest toml-test-error:inline-table-duplicate-key-simple ()
  "Same key defined twice."
  (should-error (toml:read-from-string "a = {b = 1, b = 2}")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:inline-table-scalar-then-dotted ()
  "Nesting into scalar."
  (should-error (toml:read-from-string "a = {b = 1, b.c = 2}")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:inline-table-dotted-then-scalar ()
  "Overwriting implicit table with scalar."
  (should-error (toml:read-from-string "a = {b.c = 1, b = 2}")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:inline-table-extend-inner ()
  "Extending inline table is rejected."
  (should-error (toml:read-from-string
                 "a = {inner = {dog = \"best\"}, inner.cat = \"worst\"}")
                :type 'toml-inline-table-immutable-error))

(ert-deftest toml-test:inline-table-dotted-keys-ok ()
  "Different leaf keys under same implicit table are allowed."
  (let ((parsed (toml:read-from-string "a = {b.a = 1, b.c = 2}")))
    (should (equal 1 (cdr (assoc "a" (cdr (assoc "b" (cdr (assoc "a" parsed))))))))
    (should (equal 2 (cdr (assoc "c" (cdr (assoc "b" (cdr (assoc "a" parsed))))))))))

(ert-deftest toml-test:multiline-string-key-rejected ()
  "Multiline basic/literal strings must not be used as keys."
  (should-error (toml:read-from-string "\"\"\"key\"\"\" = 1")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "'''key''' = 1")
                :type 'toml-key-error))

(ert-deftest toml-test:multiline-string-table-key-rejected ()
  "Multiline basic/literal strings must not be used as table keys."
  (should-error (toml:read-from-string "[\"\"\"key\"\"\"]")
                :type 'toml-table-error)
  (should-error (toml:read-from-string "['''key''']")
                :type 'toml-table-error))

(ert-deftest toml-test:key-without-value-rejected ()
  "Keys without values must be rejected."
  (should-error (toml:read-from-string "key = \n")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "key =\n1")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "key =")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "key = # INVALID")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "key =\t# INVALID")
                :type 'toml-key-error))

(ert-deftest toml-test:extra-tokens-after-value-rejected ()
  "Extra tokens after a key-value pair on the same line must be rejected."
  (should-error (toml:read-from-string "key = 1 foo")
                :type 'toml-key-error)
  (should-error (toml:read-from-string "key = \"val\" extra")
                :type 'toml-key-error))

(ert-deftest toml-test:extra-tokens-after-table-header-rejected ()
  "Extra tokens after a table header on the same line must be rejected."
  (should-error (toml:read-from-string "[table] foo")
                :type 'toml-table-error)
  (should-error (toml:read-from-string "[[array]] bar")
                :type 'toml-table-error))

(ert-deftest toml-test-error:array-table-conflicts-with-single-table ()
  "Array table [[tbl]] must error if [tbl] was already defined."
  (should-error (toml:read-from-string "[tbl]\n[[tbl]]")
                :type 'toml-array-table-error))

(ert-deftest toml-test-error:dotted-redefine-table-false ()
  "Dotted key must not redefine a false value as a table."
  (should-error (toml:read-from-string "a = false\na.b = true")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:overwrite-with-deep-table ()
  "Table header with deep path must not overwrite a scalar at an intermediate key."
  (should-error (toml:read-from-string "a=1\n[a.b.c.d]")
                :type 'toml-redefine-key-error))

(ert-deftest toml-test-error:append-with-dotted-keys-01 ()
  "Dotted keys must not add to an explicitly defined table."
  (should-error (toml:read-from-string "[a.b.c]\nz = 9\n\n[a]\nb.c.t = \"x\"")
                :type 'toml-redefine-table-error))

(ert-deftest toml-test-error:append-with-dotted-keys-02 ()
  "Dotted keys must not add to an explicitly defined deep table."
  (should-error (toml:read-from-string "[a.b.c.d]\nz = 9\n\n[a]\nb.c.d.k.t = \"x\"")
                :type 'toml-redefine-table-error))

;;; --- Encoding validation tests ---

(defun toml-test:write-unibyte-file (file &rest bytes)
  "Write raw BYTES to FILE as a unibyte file."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (apply #'insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region (point-min) (point-max) file nil 'silent))))

(ert-deftest toml-test-error:encoding-utf16-bom ()
  "UTF-16 BOM should be rejected."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file file #xFE #xFF #x00 #x23)
          (should-error (toml:read-from-file file)
                        :type 'toml-encoding-error))
      (delete-file file))))

(ert-deftest toml-test-error:encoding-bad-utf8-truncated ()
  "Truncated UTF-8 sequence should be rejected."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file file ?# ?\s #xC3 ?\n)
          (should-error (toml:read-from-file file)
                        :type 'toml-encoding-error))
      (delete-file file))))

(ert-deftest toml-test-error:encoding-surrogate-codepoint ()
  "Surrogate codepoint U+D800 in raw bytes should be rejected."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file file ?# ?\s #xED #xA0 #x80 ?\n)
          (should-error (toml:read-from-file file)
                        :type 'toml-encoding-error))
      (delete-file file))))

(ert-deftest toml-test-error:encoding-bad-utf8-in-string ()
  "Invalid UTF-8 in basic string should be rejected."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file file ?b ?a ?d ?\s ?= ?\s ?\" #xC3 ?\" ?\n)
          (should-error (toml:read-from-file file)
                        :type 'toml-encoding-error))
      (delete-file file))))

(ert-deftest toml-test:encoding-valid-utf8 ()
  "Valid UTF-8 multibyte characters should parse fine."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "key = \"日本語\"\n"))
          (let ((result (toml:read-from-file file)))
            (should (equal (cdr (assoc "key" result)) "日本語"))))
      (delete-file file))))

(ert-deftest toml-test:encoding-utf8-bom-valid ()
  "UTF-8 BOM at file start should be accepted and stripped."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file
           file #xEF #xBB #xBF  ;; UTF-8 BOM
           ?k ?e ?y ?\s ?= ?\s ?\" ?a ?\" ?\n)
          (let ((result (toml:read-from-file file)))
            (should (equal (cdr (assoc "key" result)) "a"))))
      (delete-file file))))

(ert-deftest toml-test-error:encoding-bom-not-at-start ()
  "BOM bytes not at file start should cause an error."
  (let ((file (make-temp-file "toml-test-" nil ".toml")))
    (unwind-protect
        (progn
          (toml-test:write-unibyte-file
           file ?# ?\s ?c ?o ?m ?m ?e ?n ?t ?\n
           #xEF #xBB #xBF  ;; BOM in middle of file
           ?k ?e ?y ?\s ?= ?\s ?\" ?a ?\" ?\n)
          ;; BOM at non-zero offset is valid UTF-8 (U+FEFF), but
          ;; the parser rejects U+FEFF as an invalid key character.
          (should-error (toml:read-from-file file)
                        :type 'toml-error))
      (delete-file file))))
