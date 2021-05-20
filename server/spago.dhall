{ name = "y-server"
, sources = [ "src/**/*.purs", "../shared/src/**/*.purs" ]
, packages = ../shared/packages.dhall
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "ordered-collections"
  , "lists"
  , "maybe"
  , "node-fs"
  , "stringutils"
  , "refs"
  , "argonaut-core"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "either"
  , "foldable-traversable"
  , "partial"
  , "prelude"
  , "strings"
  , "bifunctors"
  , "arrays"
  , "node-buffer"
  , "debuggest"
  ]
}
