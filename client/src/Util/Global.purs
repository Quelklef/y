module Y.Client.Util.Global (global) where

-- We're about to do some crazy shit

foreign import global :: forall a. String -> (forall x. x -> a) -> a
