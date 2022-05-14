{ name = "format"
, dependencies =
  [ "control"
  , "effect"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "strings"
  , "test-unit"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
