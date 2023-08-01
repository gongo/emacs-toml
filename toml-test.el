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
  (dolist (char '("\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\\/" "\\\\" "\\u1234"))
    (toml-test:buffer-setup
     char
     (should (equal char (toml:read-escaped-char)))
     (should (toml:end-of-line-p)))))

(ert-deftest toml-test-error:read-escaped-char ()
  (dolist (char '(" " " \\b" "a" "\\a" "\\c" "\\uABC!" "\\u____"))
    (toml-test:buffer-setup
     char
     (should-error (toml:read-escaped-char) :type 'toml-string-escape-error))))

(ert-deftest toml-test:read-string ()
  (toml-test:buffer-setup
   "\"GitHub Cofounder & CEO\\nLikes tater tots and beer.\""
   (should (equal "GitHub Cofounder & CEO\\nLikes tater tots and beer." (toml:read-string)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-string ()
  (dolist (str '("aiueo"       ;; Not start with '"'
                 "\"hogehoge"  ;; Not end with '"'
                 " \"aiueo\""  ;; Not start with '"'
                 ))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-string) :type 'toml-string-error))))

(ert-deftest toml-test-error:read-boolean ()
  (toml-test:buffer-setup
   "true"
   (should (equal t (toml:read-boolean)))
   (should (toml:end-of-line-p)))

  (toml-test:buffer-setup
   "false"
   (should (equal :false (toml:read-boolean)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-boolean-error ()
  (toml-test:buffer-setup
   "truu"
   (should-error (toml:read-boolean) :type 'toml-boolean-error))

  (toml-test:buffer-setup
   " false"
   (should-error (toml:read-boolean) :type 'toml-boolean-error)))

(ert-deftest toml-test:read-datetime ()
  (toml-test:buffer-setup
   "1979-05-27T07:32:00Z"
   (should (equal '(0 32 7 27 5 1979) (toml:read-datetime)))
   (should (toml:end-of-line-p))))

(ert-deftest toml-test-error:read-datetime ()
  (dolist (str '("1979-05-27" "1979-35-27T07:32:00Z" " 1979-05-27T07:32:00Z"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-datetime) :type 'toml-datetime-error))))

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
     (should (floatp numeric)))))

(ert-deftest toml-test-error:read-numeric ()
  (dolist (str '("" "+11" "- 1.1" " 1.1" ".1" "1.1.1" "1.1.1.1"))
    (toml-test:buffer-setup
     str
     (should-error (toml:read-numeric) :type 'toml-numeric-error))))

(ert-deftest toml-test:read-key ()
  (toml-test:buffer-setup
   "a = 3"
   (should (equal "a" (toml:read-key)))
   (should (eq ?3 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "biueo = true"
   (should (equal "biueo" (toml:read-key)))
   (should (eq ?t (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "connection_max = 5000"
   (should (equal "connection_max" (toml:read-key)))
   (should (eq ?5 (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "connection-max = name"
   (should (equal "connection-max" (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "server12 = name"
   (should (equal "server12" (toml:read-key)))
   (should (eq ?n (toml:get-char-at-point)))))

(ert-deftest toml-test-error:read-key ()
  ;; no key
  (toml-test:buffer-setup
   " = 3"
   (should-error (toml:read-key) :type 'toml-key-error))

  ;; key only
  (toml-test:buffer-setup
   "key"
   (should-error (toml:read-key) :type 'toml-key-error))

  ;; start with number.
  (toml-test:buffer-setup
   "1biueo = true"
   (should-error (toml:read-key) :type 'toml-key-error))

  ;; end with underscore
  (toml-test:buffer-setup
   "connection_ = 5000"
   (should-error (toml:read-key) :type 'toml-key-error)))


(ert-deftest toml-test:read-keygroup ()
  (toml-test:buffer-setup
   "[aiueo]"
   (should (equal '("aiueo") (toml:read-keygroup))))

  (toml-test:buffer-setup
   "[ai-ueo]"
   (should (equal '("ai-ueo") (toml:read-keygroup))))

  (toml-test:buffer-setup
   "[servers]
    [servers.alpha]

       key = value"
   (should (equal '("servers" "alpha") (toml:read-keygroup)))
   (should (eq ?k (toml:get-char-at-point))))

  (toml-test:buffer-setup
   "[servers]
    [servers.alpha]
    [client]"
   (should (equal '("client") (toml:read-keygroup)))))

(ert-deftest toml-test-error:read-keygroup ()
  (toml-test:buffer-setup
   "[]"
   (should-error (toml:read-keygroup) :type 'toml-keygroup-error))

  ;; end with underscore "_"
  (toml-test:buffer-setup
   "[foo.bar_]"
   (should-error (toml:read-keygroup) :type 'toml-keygroup-error))

  ;; end with period "."
  (toml-test:buffer-setup
   "[foo.bar.]"
   (should-error (toml:read-keygroup) :type 'toml-keygroup-error)))

(ert-deftest toml-test:make-hashes ()
  (let (hash)
    (setq hash (toml:make-hashes '("servers" "alpha") "ip" "192.0.2.1" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))

    (setq hash (toml:make-hashes '("servers" "alpha") "dc" "eqdc10" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))

    (setq hash (toml:make-hashes '("servers" "beta") "dc" "eqdc10" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "beta"  "dc") hash)))

    (setq hash (toml:make-hashes '("client") "ip" "192.0.2.123" hash))
    (should (equal '("ip" . "192.0.2.1") (toml:assoc '("servers" "alpha" "ip") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "alpha" "dc") hash)))
    (should (equal '("dc" . "eqdc10")    (toml:assoc '("servers" "beta"  "dc") hash)))
    (should (equal '("ip" . "192.0.2.123") (toml:assoc '("client" "ip") hash)))

    ;; update
    (setq hash (toml:make-hashes '("servers" "alpha") "ip" "192.0.2.233" hash))
    (should (equal '("ip" . "192.0.2.233") (toml:assoc '("servers" "alpha" "ip") hash)))))

(ert-deftest toml-test:parse()
  (toml-test:buffer-setup
   "\
\[a.b\]
c = 1
g = { bar = 4711, baz = \"foo\" }

\[a\]
d = 2
e = [1, 2]
f = { foo = 2342 }
"
   (let ((parsed (toml:read)))
     (should (equal '("c" . 1) (toml:assoc '("a" "b" "c") parsed)))
     (should (equal '("d" . 2) (toml:assoc '("a" "d") parsed)))
     (should (equal '("e" . [1 2]) (toml:assoc '("a" "e") parsed)))
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

(ert-deftest toml-test:parse-empty ()
  (toml-test:buffer-setup
   "\
\[empty\]
l = []
d = {}
f = false
"
   (let ((parsed (toml:read)))
     (should (equal '("l" . []) (toml:assoc '("empty" "l") parsed)))
     (should (equal '("d" . nil) (toml:assoc '("empty" "d") parsed)))
     (should (equal '("f" . :false) (toml:assoc '("empty" "f") parsed)))
     )))


(ert-deftest toml-test-error:parse ()
  (toml-test:buffer-setup
   "\
\[a\]
b = 1

\[a\]
c = 2"
   (should-error (toml:read) :type 'toml-redefine-keygroup-error))

  (toml-test:buffer-setup
   "\
\[a\]
b = 1

\[a.b\]
c = 2"
   (should-error (toml:read) :type 'toml-redefine-key-error))
)
