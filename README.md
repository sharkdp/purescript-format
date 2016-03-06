# purescript-format

Type-safe, printf-style formatting for PureScript.

## Usage
``` purs
> formatInt (width 6) 123
"   123"

> formatInt (signed <> width 6) 123
"  +123"

> formatInt (signed <> width 6 <> fillZeros) (-123)
"-00123"
```
