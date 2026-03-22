emacs-toml
==========

[![Main workflow](https://github.com/gongo/emacs-toml/workflows/Main%20workflow/badge.svg)](https://github.com/gongo/emacs-toml/actions?query=workflow%3A%22Main+workflow%22)

`toml.el` is a library for parsing TOML (Tom's Obvious, Minimal Language).

* Learn all about TOML here: https://github.com/mojombo/toml
* Supported version: [v1.1.0](https://github.com/toml-lang/toml/releases/tag/1.1.0)

## Versioning

emacs-toml uses the version format `x.y.z.N`.

- `x.y.z` — The version of the [TOML specification](https://toml.io/) that emacs-toml supports.
- `.N` — The release number of emacs-toml itself. This number increments when there are changes to emacs-toml (bug fixes, internal improvements, etc.) without a change in the supported TOML specification version.

For example, `1.0.0.0` is the initial release supporting TOML v1.0.0, and `1.0.0.1` would be the first patch release with bug fixes or improvements.

## Type Mapping

| TOML type | Emacs Lisp | Example |
|---|---|---|
| String | `string` | `"foo"` → `"foo"` |
| Integer | `integer` | `42` → `42` |
| Float | `float` | `3.14` → `3.14` |
| Boolean (`true`) | `t` | `true` → `t` |
| Boolean (`false`) | `nil` | `false` → `nil` |
| Offset Date-Time | alist | `1979-05-27T07:32:00Z` → `((year . 1979) (month . 5) ...)` |
| Local Date-Time | alist | `1979-05-27T07:32:00` → `((year . 1979) (month . 5) ...)` |
| Local Date | alist | `1979-05-27` → `((year . 1979) (month . 5) (day . 27))` |
| Local Time | alist | `07:32:00` → `((hour . 7) (minute . 32) (second . 0) ...)` |
| Array | vector | `[1, 2]` → `[1 2]` |
| Empty Array | `[]` (empty vector) | `[]` → `[]` |
| Table | alist | `{a = 1}` → `(("a" . 1))` |
| Empty Table | `:toml-empty-table` | `{}` → `:toml-empty-table` |
| Array of Tables | vector of alists (or `:toml-empty-table` for empty elements) | `[[products]]` ... → `[(("name" . "Hammer")) ...]` |
| `inf`, `+inf` | `1.0e+INF` | `inf` → `1.0e+INF` |
| `-inf` | `-1.0e+INF` | `-inf` → `-1.0e+INF` |
| `nan`, `+nan` | `0.0e+NaN` | `nan` → `0.0e+NaN` |
| `-nan` | `-0.0e+NaN` | `-nan` → `-0.0e+NaN` |

> [!NOTE]
> Empty tables (`{}`) are represented as the `:toml-empty-table` symbol to distinguish them from `nil`. See [#103](https://github.com/gongo/emacs-toml/pull/103) for background.

> [!NOTE]
> Datetime alist keys vary by type: `timezone` is present only in Offset Date-Time, and `fraction` is included in date/time types.

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

## Test

### Unit Tests

Run the unit tests with:

```bash
make test
```

### Official Test Suite

This project also includes tests using the official [toml-lang/toml-test](https://github.com/toml-lang/toml-test) suite as a git submodule.

To run the official test suite:

```bash
# Initialize the submodule (first time only)
git submodule update --init

# Run tests
make test-official
```

## License

MIT License. see `toml.el`.
