let upstream = https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f
in upstream //
  { elmish =
    { repo = "https://github.com/ursi/purescript-elmish.git"
    , version = "b5f80c0d3cec0de0b8bea38816a054c03ae30390"
    , dependencies =
      [ "console"
      , "foreign-object"
      , "heterogeneous"
      , "js-timers"
      , "parallel"
      , "task"
      , "debuggest"
      , "whatwg-html"
      , "murmur3"
      , "mason-prelude"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    }
  , debuggest =
    { repo = "https://github.com/ursi/purescript-debuggest.git"
    , version = "eeca2daa8b5232f54e14ef6d9d4cc8ffc80f3c93"
    , dependencies = [ "prelude", "unsafe-coerce" ]
    }
  , whatwg-html =
    { repo = "https://github.com/ursi/purescript-whatwg-html.git"
    , version = "8a2b9bb79abdb50fed1f74780003b50019e5e748"
    , dependencies = [ "ffi-options", "mason-prelude", "nullable" ]
    }
  , murmur3 =
    { repo = "https://github.com/ursi/purescript-murmur3"
    , dependencies = [ "prelude", "functions" ]
    , version = "4da7d071ac5791b21fe9064d84e067a34fdc29e3"
    }
  , ffi-options =
    { dependencies = [] : List Text
    , repo = "https://github.com/ursi/purescript-ffi-options.git"
    , version = "568f213577549e958f931f4d9e7dc7c57bf5fadc"
    }
  , mason-prelude =
    { repo = "https://github.com/ursi/purescript-mason-prelude.git"
    , version = "d447a79e8b44e509cb2cd96e3ce62a5024d1dda2"
    , dependencies =
      [ "arrays"
      , "console"
      , "either"
      , "functions"
      , "integers"
      , "lists"
      , "math"
      , "parallel"
      , "point-free"
      , "prelude"
      , "debuggest"
      , "strings"
      , "tuples"
      , "unfoldable"
      ] 
    }
  , task =
    { repo = "https://github.com/ursi/purescript-task.git"
    , version = "e5c64a850962055b809580ea2aba99c6f0d9cca7"
    , dependencies = [ "mason-prelude", "js-timers" ]
    }
  }
