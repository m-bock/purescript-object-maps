{ name = "object-maps"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "either"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "st"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
