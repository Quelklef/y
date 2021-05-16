module Server.Database (load, push) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.String.Utils (endsWith)
import Data.String.CodeUnits (slice)
import Data.Traversable (for, sequence)
import Data.Newtype (unwrap)
import Data.Array (catMaybes, toUnfoldable)
import Node.FS.Sync (readdir, readFile, writeFile)
import Node.Buffer (Buffer)

import Shared.Id (Id, parseId)
import Shared.Codable (class Encodable, class Decodable, encode, decode)

-- Our "database" will be the filesystem
-- Why? Meh, why not.
-- Since the app architecture is append-only, we can kinda get away with it (for now).

-- | Load all events in a conversation
load :: forall m event (cid :: Symbol). Applicative m => Decodable m event => Id cid -> Effect (m (List event))
load cid = do
  let dirLoc = "./db/" <> unwrap cid
  fileNames <- readdir dirLoc
  let eids =
        fileNames
        # map (\fileName ->
            if fileName # endsWith ".txt"
            then parseId { namespace: Nothing } =<< (fileName # slice 0 (-4))
            else Nothing)
        # catMaybes
  mEvents :: Array (m event) <- for eids \eid -> do
    let fileLoc = "./db/" <> unwrap cid <> "/" <> unwrap eid <> ".txt"
    mEvent <- decode <<< decodeUtf8 <$> readFile fileLoc
    pure mEvent
  pure $ mEvents # toUnfoldable # sequence

-- | Save an event into a conversation
push :: forall event (cid :: Symbol) (eid :: Symbol). Encodable event => Id cid -> Id eid -> event -> Effect Unit
push cid eid event = do
  let fileLoc = "./db/" <> unwrap cid <> "/" <> unwrap eid <> ".txt"
  writeFile fileLoc (encodeUtf8 <<< encode $ event)

foreign import encodeUtf8 :: String -> Buffer
foreign import decodeUtf8 :: Buffer -> String
