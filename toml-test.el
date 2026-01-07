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
     (should (integerp numeric)))))

(ert-deftest toml-test-error:read-numeric ()
  (dolist (str '("" "- 1.1" " 1.1" ".1" "1.1.1" "1.1.1.1"))
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
  )

(ert-deftest toml-test-error:read-array ()
  (toml-test:buffer-setup
   "[1 2 3]"
   (should-error (toml:read-array) :type 'toml-array-error))

  ;; FIXME: Data types may not be mixed
  ;; (toml-test:buffer-setup
  ;;  "[1, 0.2]"
  ;;  (should-error (toml:read-array) :type 'toml-array-error))
  )

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
  )

(ert-deftest toml-test-error:read-table ()
  (toml-test:buffer-setup
   "[]"
   (should-error (toml:read-table) :type 'toml-table-error))

  ;; end with underscore "_"
  (toml-test:buffer-setup
   "[foo.bar_]"
   (should-error (toml:read-table) :type 'toml-table-error))

  ;; end with period "."
  (toml-test:buffer-setup
   "[foo.bar.]"
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
                   ;; TODO: perhaps add unsupported err?
                  ("'''"                    . toml-key-error)))
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
       ;; Second element is empty (nil)
       (should (null (aref products 1)))
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
