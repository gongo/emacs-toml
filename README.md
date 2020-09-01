emacs-toml
==========

[![Main workflow](https://github.com/gongo/emacs-toml/workflows/Main%20workflow/badge.svg)](https://github.com/gongo/emacs-toml/actions?query=workflow%3A%22Main+workflow%22)

`toml.el` is a library for parsing TOML (Tom's Obvious, Minimal Language).

* Learn all about TOML here: https://github.com/mojombo/toml
* Support version: [v0.1.0](https://github.com/mojombo/toml/blob/master/versions/toml-v0.1.0.md)

## Example

Parse the [example.toml](https://github.com/mojombo/toml/blob/master/tests/example.toml) as an example.

```lisp
(toml:read-from-string "\
key1 = \"foo\"
key2 = \"bar\"
key3 = \"333\"")

;; => '(("key3" . "333") ("key2" . "bar") ("key1" . "foo"))
```

```lisp
(toml:read-from-file "example.toml")

;; or

(toml:read-from-string "\
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
\]")

;; =>  '(
;;        ("clients"
;;          ("hosts" "alpha" "omega")
;;          ("data" ("gamma" "delta") (1 2)))
;;        ("servers"
;;          ("beta" ("dc" . "eqdc10") ("ip" . "10.0.0.2"))
;;          ("alpha" ("dc" . "eqdc10") ("ip" . "10.0.0.1")))
;;        ("database"
;;          ("enabled" . t)
;;          ("connection_max" . 5000)
;;          ("ports" 8001 8001 8002)
;;          ("server" . "192.168.1.1"))
;;        ("owner"
;;          ("dob" 0 32 7 27 5 1979)
;;          ("bio" . "GitHub Cofounder & CEO\\nLikes tater tots and beer.")
;;          ("organization" . "GitHub")
;;          ("name" . "Tom Preston-Werner"))
;;        ("title" . "TOML Example"))
```

## Spec

In `emacs-toml`, "key groups" and "key" key pattern are as follows:

* `key` = `[a-zA-Z][a-zA-Z0-9_]*`
* `table` = `[a-zA-Z][a-zA-Z0-9_\\.]*`
    * The end doesn't end in the period.

## Test

Use [Cask.el](https://github.com/rejeep/cask.el). follow commands:

```
$ make test
cask exec emacs -Q --batch \
		--load toml.el \
		--load toml-test.el \
		-f ert-run-tests-batch-and-exit
Real cl-lib shadowed by compatibility cl-lib? (/Users/gongo/.emacs.d/elpa/cl-lib-0.3/cl-lib.elc)
Real cl-lib shadowed by compatibility cl-lib? (/Users/gongo/.emacs.d/elpa/cl-lib-0.3/cl-lib.elc)
Running 21 tests (2013-08-29 22:33:46+0900)
   passed   1/21  toml-test-error:parse
   passed   2/21  toml-test-error:read-boolean
   passed   3/21  toml-test-error:read-datetime
   passed   4/21  toml-test-error:read-escaped-char
   passed   5/21  toml-test-error:read-key
   passed   6/21  toml-test-error:read-table
   passed   7/21  toml-test-error:read-numeric
   passed   8/21  toml-test-error:read-string
   passed   9/21  toml-test:make-hashes
Mark set
Mark set
Mark set
Mark set
Mark set
   passed  10/21  toml-test:parse
   passed  11/21  toml-test:read-char
   passed  12/21  toml-test:read-char-with-char-p
   passed  13/21  toml-test:read-datetime
   passed  14/21  toml-test:read-escaped-char
   passed  15/21  toml-test:read-key
   passed  16/21  toml-test:read-table
   passed  17/21  toml-test:read-numeric
   passed  18/21  toml-test:read-string
   passed  19/21  toml-test:seek-beginning-of-next-line
   passed  20/21  toml-test:seek-non-whitespace
   passed  21/21  toml-test:seek-readable-point

Ran 21 tests, 21 results as expected (2013-08-29 22:33:46+0900)
```

## License

MIT License. see `toml.el`.
