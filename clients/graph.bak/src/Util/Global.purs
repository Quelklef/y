module Y.Client.Util.Global (global) where

import Data.Lazy (Lazy, force)

-- We're about to do some crazy shit

global :: forall a. String -> Lazy a -> a
global key lz = global_f key (\_ -> force lz)

foreign import global_f :: forall a. String -> (forall x. x -> a) -> a
