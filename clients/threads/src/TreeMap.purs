module TreeMap
  ( IVP
  , VPC
  , Thread
  , TreeMap
  , edit
  , empty
  , findLeaf
  , findNewLeaf
  , getThread
  , getThreads
  , isLeaf
  , leaves
  , lookup
  , member
  , removeLeaf
  , removeLeafRecursive
  , siblings
  , toTreeMap
  )
  where

import MasonPrelude
import Data.Array as Array
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEList
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set

type IVP a b =
  { id :: a
  , value :: b
  , parent :: Maybe a
  }

type VPC a b =
  { value :: b
  , parent :: Maybe a
  , children :: Array a
  }

newtype TreeMap a b = TreeMap
  { leaves :: Map a (VPC a b)
  , parents :: Map a (Either (Array a) (VPC a b))
  }

toTreeMap :: ∀ a b. Ord a => List (IVP a b) -> TreeMap a b
toTreeMap =
  TreeMap
  <. foldl
       (\acc@{ parents } { id, value, parent } ->
          -- deal with parent
          case parent of
            Just pid ->
              case Map.lookup pid acc.leaves of
                Just parent' ->
                  { leaves: Map.delete pid acc.leaves
                  , parents:
                      Map.insert
                        pid
                        (Right $ parent' { children = [ id ] })
                        parents
                  }

                Nothing ->
                  case Map.lookup pid parents of
                    Just (Left children) ->
                      acc
                        { parents =
                            Map.insert
                              pid
                              (Left $ Array.snoc children id)
                              parents
                        }

                    Just (Right parent') ->
                      acc
                        { parents =
                            Map.insert
                              pid
                              (Right
                               $ parent' { children = Array.snoc parent'.children id }
                              )
                              parents
                        }

                    Nothing -> acc { parents = Map.insert pid (Left [ id ]) parents }

            Nothing -> acc

          -- insert the value
          # \acc' ->
              case Map.lookup id acc'.parents of
                Just (Left children) ->
                  acc'
                    { parents =
                        Map.insert
                          id
                          (Right
                             { value
                             , parent
                             , children
                             }
                          )
                          acc'.parents
                    }

                Nothing ->
                  acc'
                    { leaves =
                        Map.insert
                          id
                          { value
                          , parent
                          , children: []
                          }
                          acc'.leaves
                    }

                _ -> unsafeThrow "Tree.purs: Something has gone wrong"
       )
                -- no monoid instance right now
       { leaves: Map.empty
       , parents: Map.empty
       }

lookup :: ∀ a b. Ord a => a -> TreeMap a b -> Maybe (VPC a b)
lookup key (TreeMap r@{ parents })=
  case Map.lookup key parents of
    Just (Right vpc) -> Just vpc
    Nothing -> Map.lookup key r.leaves
    _ -> unsafeThrow "Tree.purs: lookup: Something has gone wrong"

lookupEither :: ∀ a b. Ord a => a -> TreeMap a b -> Either String (VPC a b)
lookupEither key (TreeMap r@{ parents })=
  case Map.lookup key parents of
    Just (Right vpc) -> Right vpc
    Nothing -> case Map.lookup key r.leaves of
      Just value -> Right value
      Nothing -> Left $ "key not found"

    _ -> unsafeThrow "Tree.purs: lookup: Something has gone wrong"

siblings :: ∀ a b. Ord a => a -> TreeMap a b -> Either String (Array b)
siblings key tm =
  let find = lookupEither ~$ tm in
  lookupEither key tm
  >>= \{ parent } ->
        case parent of
          Just p ->
            find p
            <#> _.children .> Array.delete key
            >>= traverse (find .> map _.value)
          Nothing -> Right []

type Thread a = NonEmptyList (a /\ Array a)

getThread :: ∀ a b. Ord a => a -> TreeMap a b -> Maybe (Thread b)
getThread start tm = go start pure <#> NEList.reverse
  where
    go :: a -> (b /\ Array b -> Thread b) -> Maybe (Thread b)
    go key f = do
      lookup key tm
      >>= \{ value, parent } ->
            case siblings key tm of
              Right sibs ->
                let f'd = f $ value /\ sibs in
                case parent of
                  Just pid -> helper f'd pid
                  Nothing -> Just f'd
              Left _ -> unsafeThrow "Tree.purs: getThread"

    helper :: Thread b -> a -> Maybe (Thread b)
    helper acc = do
      go ~$ (NEList.cons ~$ acc)

getThreads :: ∀ a b. Ord a => TreeMap a b -> Array (Thread b)
getThreads tm@(TreeMap r) =
  Map.keys r.leaves
  # Set.toUnfoldable
  # traverse (getThread ~$ tm)
  # case _ of
      Just a -> a
      Nothing -> unsafeThrow "Tree.purs: getThreads"

leaves :: ∀ a b. TreeMap a b -> Array (a /\ VPC a b)
leaves (TreeMap r) =  Map.toUnfoldable r.leaves

findLeaf :: ∀ a b. Ord a => a -> TreeMap a b -> Maybe a
findLeaf id tm =
  lookup id tm
  >>= \{ children } ->
        case Array.uncons children of
          Just { head } -> findLeaf head tm
          Nothing -> Just id

empty :: ∀ a b. TreeMap a b
empty = TreeMap { leaves: Map.empty, parents: Map.empty }

edit :: ∀ a b. Ord a => a -> (VPC a b -> VPC a b) -> TreeMap a b -> TreeMap a b
edit key f tm@(TreeMap r) =
  case Map.lookup key r.leaves of
    Just vpc ->
      TreeMap $ r { leaves = Map.insert key (f vpc) r.leaves }

    Nothing ->
      case Map.lookup key r.parents of
        Just (Right vpc) ->
          TreeMap $ r { parents = Map.insert key (Right (f vpc)) r.parents }

        _ -> tm

isLeaf :: ∀ a b. Ord a => a -> TreeMap a b -> Boolean
isLeaf key tm =
  lookup key tm
  # maybe false (_.children .> Array.null)

type Move = Boolean

removeLeaf :: ∀ a b. Ord a => a -> TreeMap a b -> TreeMap a b
removeLeaf key tm@(TreeMap r) =
  case Map.lookup key r.leaves of
    Just vpc ->
      let
        potentialNewLeaves = Map.delete key r.leaves

        newParentData :: Maybe (Move /\ a /\ Either (Array a) (VPC a b))
        newParentData =
          vpc.parent
          <#> \pid ->
                case Map.lookup pid r.parents of
                  Just (Right vpc') ->
                    let newChildren = Array.delete key vpc'.children in
                    (newChildren == [])
                    /\ pid
                    /\ Right (vpc' { children = newChildren })
                  _ -> unsafeThrow "removeLeaf"
                -- Map.update
                --   (case _ of
                --      Right vpc' ->
                --        Just $ Right $ vpc' { children = Array.delete key vpc'.children }

                --      Left children -> Just $ Left $ Array.delete key children
                --   )
                --   pid
                --   r.parents

      in
      TreeMap
        case newParentData of
          Just (true /\ pid /\ (Right vpc')) ->
            { leaves: Map.insert pid vpc' potentialNewLeaves
            , parents: Map.delete pid r.parents
            }

          Just (false /\ pid /\ parent) ->
            { leaves: potentialNewLeaves
            , parents: Map.insert pid parent r.parents
            }

          Nothing ->
            { leaves: potentialNewLeaves
            , parents: r.parents
            }
          _ -> unsafeThrow "removeLeaf 2"


    Nothing -> tm

removeLeafRecursive :: ∀ a b.
  Ord a
  => (VPC a b -> Boolean)
  -> a
  -> TreeMap a b
  -> TreeMap a b
removeLeafRecursive shouldRemove key tm =
  case lookup key tm of
    Just vpc ->
      if vpc.children == [] && shouldRemove vpc then
        let newTm = removeLeaf key tm in
        case vpc.parent of
          Just pid -> removeLeafRecursive shouldRemove pid newTm
          Nothing -> newTm
      else
        tm

    Nothing -> tm

member :: ∀ a b. Ord a => a -> TreeMap a b -> Boolean
member key tm = lookup key tm # maybe false \_ -> true

-- | For when a leaf has been recursively deleted
findNewLeaf :: ∀ a b. Ord a => a -> TreeMap a b -> TreeMap a b -> Maybe a
findNewLeaf key oldTM newTM =
  if member key newTM then
    findLeaf key newTM
  else
    lookup key oldTM
    >>= _.parent
    >>= findNewLeaf ~$ oldTM ~$ newTM
