# purescript-format

Type-safe, printf-style formatting for PureScript.

## Usage
``` purs
> format (width 6) 123
"   123"

> format (signed <> width 6) 123
"  +123"

> format (zeroFill <> width 6) (-123)
"-00123"

> format (width 8 <> precision 3) pi
"   3.142"

> format (width 8 <> precision 3) 10.0
"  10.000"

> format (width 8) "foo"
"     foo"
```
