{ name = "object-maps"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-core"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
