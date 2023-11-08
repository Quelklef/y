module Y.Client.Util.Is (is, (===)) where

foreign import is :: forall a b. a -> b -> Boolean
infixl 4 is as ===
