emacs-toml
==========

[![Main workflow](https://github.com/gongo/emacs-toml/workflows/Main%20workflow/badge.svg)](https://github.com/gongo/emacs-toml/actions?query=workflow%3A%22Main+workflow%22)

`toml.el` is a library for parsing TOML (Tom's Obvious, Minimal Language).

* Learn all about TOML here: https://github.com/mojombo/toml
* Support version: [v0.2.0](https://github.com/toml-lang/toml/blob/main/CHANGELOG.md#020--2013-09-24)

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
;;       ("clients"
;;        ("hosts" . ["alpha" "omega"])
;;        ("data" . [["gamma" "delta"] [1 2]]))
;;       ("servers"
;;        ("beta" ("dc" . "eqdc10") ("ip" . "10.0.0.2"))
;;        ("alpha" ("dc" . "eqdc10") ("ip" . "10.0.0.1")))
;;       ("database"
;;        ("enabled" . t)
;;        ("connection_max" . 5000)
;;        ("ports" . [8001 8001 8002])
;;        ("server" . "192.168.1.1"))
;;       ("owner"
;;        ("dob"
;;         (year . 1979)
;;         (month . 5)
;;         (day . 27)
;;         (hour . 7)
;;         (minute . 32)
;;         (second . 0)
;;         (fraction)
;;         (timezone . "Z"))
;;        ("bio" . "GitHub Cofounder & CEO\\nLikes tater tots and beer.")
;;        ("organization" . "GitHub")
;;        ("name" . "Tom Preston-Werner"))
;;       ("title" . "TOML Example"))
```

## Spec

In `emacs-toml`, "key groups" and "key" key pattern are as follows:

* `key` = `[a-zA-Z][a-zA-Z0-9_]*`
* `keygroup` = `[a-zA-Z][a-zA-Z0-9_\\.]*`
    * The end doesn't end in the period.

## Test

### Unit Tests

Run the unit tests with:

```bash
make test
```

### Official Test Suite

This project also includes tests using the official [toml-lang/toml-test](https://github.com/toml-lang/toml-test) suite as a git submodule.

**Note:** Since `emacs-toml` currently supports TOML v0.1.0, some tests from the official suite (which targets TOML v1.1.0) will fail. This is expected.

To run the official test suite:

```bash
# Initialize the submodule (first time only)
git submodule update --init

# Run tests
make test-official
```

## License

MIT License. see `toml.el`.
