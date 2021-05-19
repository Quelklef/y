{ name = "y-client"
, sources = [ "src/**/*.purs", "../shared/src/**/*.purs" ]
, packages = ../shared/packages.dhall
, dependencies =
  [ "effect"
  , "psci-support"
  , "elmish"
  , "ordered-collections"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "transformers"
  , "tuples"
  ] 
}
