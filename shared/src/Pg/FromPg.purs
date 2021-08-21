module Y.Shared.Pg.FromPg
  ( class FromPg
  , Impl
  , mkImpl
  , fromPg

  -- v Impl details forced to be exported by ps
  , impl
  , class InnerTup
  , impl_inner

  ) where

-- Must be in shared/ so that shared types can implement FromPg

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(..)) as String
import Data.List (List)
import Data.List as List
import Data.Array (uncons) as Array
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Int (fromString) as Int
import Data.Number (fromString) as Number
import Data.Newtype (un)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Either (Either(..))
import Data.Bifunctor (lmap)
import Data.Set (Set)
import Data.Set (fromFoldable) as Set

import Y.Shared.Util.MonadJuggle (class MonadJuggle, fromEither)
import Y.Shared.Pg.Types (Tup(..), PgExpr(..))
import Y.Shared.Pg.Internal.ParseComposite (parseComposite) as PC

-- | Class of types which can be parsed out of SQL expressions
class FromPg a where
  impl :: Impl a
-- Keep the type opaque because:
--  a) Since the set of SQL expressions is a known domain, the
--     responsibility of parsing them should fall on this
--     library code. The client should not have to--or even
--     be allowed to--think about SQL expressions.
--  b) Restricting access ot the SQL expressions gives better
--     future compatibility for becoming database-polymorphic.

newtype Impl a = Impl
  { parser :: ExprParser a
  , typename :: String
  }

type ExprParser a = PgExpr -> Either ParseErr a
-- Morally a String -> Either err res
--
-- Note that
--   String -> Either err res                           [1]
-- is a strange type for a parser; more common is something like
--   String -> Either err { res: res, rest: String }    [2]
-- The reason for using type [1] instead of [2] is as follows.
--
-- Most modern grammars exhibit behaviour wherein in order to parse
-- the structure of composite types, the elements must be parsed as
-- well. For instance, the Purescript array
--   [ "one, two", "three" ]
-- Cannot be parsed without parsing the contained strings, because
-- by parsing the strings we learn that the first comma is *not*
-- a part of the array syntax; there are two elements, not three.
-- Grammar like this demands parser types of form [2].
--
-- On the contrary, PostgreSQL grammar *does* allow for containers
-- to be parsed before their elements. We can parse the array string
--   {<(0\,0)\,1>,<(1\,1)\,2>}
-- into the element strings
--   <(0,0),1>  and  <(1,1),2>
-- before knowing what type they are inteded to be (CIRCLE? TEXT?).
-- Because of this grammatical quirk, we are able to get away with
-- using the simpler parser type [1].

type ParseErr =
  { issue :: String  -- the actual error
  , context :: List String  -- messages, with earlier ones contextualizing later ones
  , culprit :: Maybe PgExpr  -- the erroneous code
  }

-- begin util --

contextualize :: forall s a. String -> (s -> Either ParseErr a) -> (s -> Either ParseErr a)
contextualize ctx parse expr = parse expr # lmap mapErr
  where mapErr err = err { context = List.Cons ctx err.context }

fail :: forall a. Maybe PgExpr -> String -> Either ParseErr a
fail culprit issue = Left { issue, culprit, context: mempty }

mkImpl :: forall a b. FromPg a => (a -> Either String b) -> Impl b
mkImpl parser =
  let Impl innerImpl = (impl :: Impl a)
      innerParser = innerImpl.parser
  in Impl
    { typename: innerImpl.typename  -- user types inherit pg typenames
    , parser:
        \expr -> do
          val <- innerParser expr
          res <- parser val # lmap \issue -> { issue, culprit: Nothing, context: List.singleton "After a successful parse" }
          pure res
    }

fromPg :: forall m a. MonadJuggle String m => FromPg a => PgExpr -> m a
fromPg expr = parser expr # lmap showErr # fromEither
  where

  Impl { parser, typename } = impl

  showErr :: ParseErr -> String
  showErr err =
      [ "Failed to parse PostgreSQL expr"
      , "Of type: " <> typename
      , "Because: " <> err.issue ]
      <>
      (case err.culprit of
        Nothing -> []
        Just (PgExpr culp) -> ["Caused by the expression: " <> culp])
      <>
      [ List.reverse err.context # map ("... " <> _) # intercalate "\n"
      , "Probable cause: an SQL row was returned that does not match the format of some FromPg instance."
      ] # intercalate "\n"

  indent :: String -> String -> String
  indent dent = String.split (String.Pattern "\n") >>> map (dent <> _) >>> intercalate "\n"

parseComposite :: { open :: String, delim :: String, close :: String } -> PgExpr -> Either ParseErr (Array PgExpr)
parseComposite opts expr = PC.parseComposite opts expr # lmap (\issue -> { issue, culprit: Just expr, context: mempty })

-- begin instances --

instance fromPg_PgExpr :: FromPg PgExpr where
  impl = Impl { typename: "postgresql expression", parser: pure }

instance fromPg_String :: FromPg String where
  impl = Impl { typename: "string", parser: un PgExpr >>> pure }

instance fromPg_Boolean :: FromPg Boolean where
  impl = Impl
    { typename: "boolean"
    , parser:
        contextualize "while parsing Boolean (BOOL)"
        $ \expr -> case un PgExpr expr of
          "t" -> pure true
          "f" -> pure false
          _ -> fail (Just expr) "Expected 't' or 'f'"
    }

instance fromPg_Number :: FromPg Number where
  impl = Impl
    { typename: "decimal number"
    , parser:
        contextualize "while parsing Number (REAL, FIXED)"
        $ \expr -> Number.fromString (un PgExpr expr) # maybe (fail (Just expr) "Bad format") pure
    }

instance fromPg_Int :: FromPg Int where
  impl = Impl
    { typename: "integral number"
    , parser:
        contextualize "while parsing Int (SMALLINT, INT, BIGINT)"
        $ \expr -> Int.fromString (un PgExpr expr) # maybe (fail (Just expr) "Bad format") pure
    }

instance fromPg_Maybe :: FromPg a => FromPg (Maybe a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "nullable " <> innerImpl.typename
      , parser: case _ of
          PgExpr "" -> pure Nothing
          expr -> Just <$> innerImpl.parser expr
      }

instance fromPg_Array :: FromPg a => FromPg (Array a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "array of " <> innerImpl.typename
      , parser:
          contextualize "while parsing Array"
          $ \expr -> do
            subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
            vals <- traverse innerImpl.parser subExprs
            pure vals
      }

instance fromPg_Set :: (Ord a, FromPg a) => FromPg (Set a) where
  impl =
    let Impl innerImpl = (impl :: Impl a)
    in Impl
      { typename: "set of " <> innerImpl.typename
      , parser:
          contextualize "while parsing Set"
          $ \expr -> do
            subExprs <- parseComposite { open: "{", delim: ",", close: "}" } expr
            vals <- traverse innerImpl.parser subExprs
            pure $ Set.fromFoldable vals
      }

instance fromPg_Tup :: InnerTup a => FromPg (Tup a) where
  impl =
    let innerImpl = (impl_inner :: InnerTup_Impl a)
    in Impl
      { typename: "tuple of (" <> innerImpl.typename <> ")"
      , parser:
          contextualize "while parsing composite type (aka row, record)"
          $ \expr -> parseComposite { open: "(", delim: ",", close: ")" } expr >>= innerImpl.parser { idx: 0 } # map Tup
      }

class InnerTup a where
  impl_inner :: InnerTup_Impl a

type InnerTup_Impl a =
    { parser :: { idx :: Int } -> Array PgExpr -> Either ParseErr a
    , typename :: String
    }

instance fromPgInnerTup_recur :: (FromPg a, InnerTup b) => InnerTup (a /\ b) where
  impl_inner =
    let Impl aImpl = (impl :: Impl a)
        aParser = aImpl.parser
        bImpl = (impl_inner :: InnerTup_Impl b)
        bParser = bImpl.parser
    in
      { typename: aImpl.typename <> ", " <> bImpl.typename
      , parser:
        \{ idx } ->
          contextualize ("while parsing element #" <> show idx)
          $ \exprs -> case Array.uncons exprs of
            Nothing -> fail Nothing "Expected an element"
            Just { head, tail } -> do
              headVal <- aParser head
              tailVal <- bParser { idx: idx + 1 } tail
              pure $ headVal /\ tailVal
      }

else instance fromPgInnerTup_empty :: InnerTup Unit where
  impl_inner =
    { typename: "<empty>" -- TODO: what to do here??? Tup0 seems to be exceptional in a number of places
    , parser:
        \_ exprs -> case Array.uncons exprs of
          Nothing -> pure unit
          Just { head, tail: _ } -> fail (Just head) "Expected no elements"
    }

else instance fromPgInnerTup_base :: FromPg a => InnerTup a where
  impl_inner =
    let Impl aImpl = (impl :: Impl a)
        aParser = aImpl.parser
    in
      { typename: aImpl.typename
      , parser:
          \_ exprs -> case exprs of
            [expr] -> aParser expr
            _ -> fail Nothing "Unexpected extraneous elements (too many values!)"
      }
