module Client.Util.Opts where

import Prelude

-- Optional arguments trick
-- Originally from https://twitter.com/paf31/status/919758040202489856?s=20

type Opts o = o -> o

defOpts :: forall o. o -> o
defOpts = identity

{- Example:

hasOptionalArgs :: Opts { flagA :: Boolean, flagB :: Boolean } -> Result
hasOptionalArgs mkOpts =
  let defaultOpts = { flagA: true, flagB: false }
      opts = mkOpts defaultOPts
  in calcResult opts

-- Call with default opts
hasOptionalArgs defOpts

-- Call with custom opts
hasOptionalArgs (_ { flagA = false })

-}
