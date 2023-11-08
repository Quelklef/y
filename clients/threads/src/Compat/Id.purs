module Compat.Id where

import MasonPrelude
import Y.Shared.Id (Id)

convertId :: ∀ a b. Id a -> Id b
convertId a = unsafeCoerce a
