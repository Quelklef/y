{ name = "y-client"
, sources = [ "src/**/*.purs", "../shared/src/**/*.purs" ]
, packages = ../shared/packages.dhall
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "elmish"
  , "ordered-collections"
  , "lists"
  , "maybe"
  , "newtype"
  , "refs"
  , "argonaut-core"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "either"
  , "foldable-traversable"
  , "partial"
  , "prelude"
  , "strings"
  , "transformers"
  , "tuples"
  , "whatwg-html"
  , "bifunctors"
  , "integers"
  , "math"
  , "control"
  , "arrays"
  , "lazy"
  ] 
}
