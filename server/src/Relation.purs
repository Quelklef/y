module Server.Relation (Relation, empty, singleton, incl, excl, union, lexp, rexp, has, lget, rget) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set

-- | Morally a @Set (l /\ r)@
data Relation l r = Relation { ltr :: Map l (Set r), rtl :: Map r (Set l) }

empty :: forall l r. Relation l r
empty = Relation { ltr: Map.empty, rtl: Map.empty }

singleton :: forall l r. Ord l => Ord r => l -> r -> Relation l r
singleton l r = Relation
  { ltr: Map.singleton l (Set.singleton r)
  , rtl: Map.singleton r (Set.singleton l)
  }

-- | Include a pairing in the relation
-- | Idempotent
incl :: forall l r. Ord l => Ord r => l -> r -> (Relation l r -> Relation l r)
incl l r = union (singleton l r)

-- | Exclude a pairing from the relation
-- | Idempotent
excl :: forall l r. Ord l => Ord r => l -> r -> (Relation l r -> Relation l r)
excl l r (Relation rel) = Relation
  { ltr: Map.update (Just <<< Set.delete r) l rel.ltr
  , rtl: Map.update (Just <<< Set.delete l) r rel.rtl
  }

union :: forall l r. Ord l => Ord r => Relation l r -> Relation l r -> Relation l r
union (Relation r0) (Relation r1) = Relation
  { ltr: Map.unionWith (<>) r0.ltr r1.ltr
  , rtl: Map.unionWith (<>) r0.rtl r1.rtl
  }

-- | Flip all relations
dual :: forall l r. Relation l r -> Relation r l
dual (Relation rel) = Relation { ltr: rel.rtl, rtl: rel.ltr }

-- | Expunge (remove all) pairings which have @l@ as the left item
lexp :: forall l r. Ord l => l -> Relation l r -> Relation l r
lexp l (Relation rel) = Relation { ltr: rel.ltr # Map.delete l, rtl: rel.rtl # map (Set.delete l) }

-- | Like @lexp@ but swapped
rexp :: forall l r. Ord r => r -> Relation l r -> Relation l r
rexp r = dual >>> lexp r >>> dual

-- | Check if a pairing exists in the relation
has :: forall l r. Ord l => Ord r => l -> r -> Relation l r -> Boolean
has l r (Relation rel) = Map.lookup l rel.ltr <#> Set.member r # fromMaybe false

-- | Get all those @r@s related to the given @l@
lget :: forall l r. Ord l => l -> Relation l r -> Set r
lget l (Relation rel) = Map.lookup l rel.ltr # fromMaybe Set.empty

-- | Like @lget@ but swapped
rget :: forall l r. Ord r => r -> Relation l r -> Set l
rget r rel = lget r (dual rel)
