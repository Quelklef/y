module Y.Server.Database (load, push) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.String.Utils (endsWith)
import Data.String.CodeUnits (slice)
import Data.Traversable (for, sequence)
import Data.Newtype (unwrap)
import Data.Array (catMaybes, toUnfoldable)
import Data.Symbol (class IsSymbol)
import Node.FS.Sync (readdir, readFile, writeFile)
import Node.Buffer (Buffer)

import Y.Shared.Util.Codable (class Encodable, class Decodable, encode, decode)
import Y.Shared.Id (Id, parseId)

-- Our "database" will be the filesystem
-- Why? Meh, why not.
-- Since the app architecture is append-only, we can kinda get away with it (for now).

-- | Load all events in a conversation
load :: forall m event (convoId :: Symbol) (eventId :: Symbol). IsSymbol eventId => Applicative m => Decodable m event => Id convoId -> Effect (m (List event))
load convoId = do
  let dirLoc = "./db/" <> unwrap convoId
  fileNames <- readdir dirLoc
  let eventIds =
        fileNames
        # map (\fileName ->
            if fileName # endsWith ".txt"
            then (parseId :: String -> Maybe (Id eventId)) =<< (fileName # slice 0 (-4))
            else Nothing)
        # catMaybes
  mEvents :: Array (m event) <- for eventIds \eventId -> do
    let fileLoc = "./db/" <> unwrap convoId <> "/" <> unwrap eventId <> ".txt"
    mEvent <- decode <<< decodeUtf8 <$> readFile fileLoc
    pure mEvent
  pure $ mEvents # toUnfoldable # sequence

-- | Save an event into a conversation
push :: forall event (convoId :: Symbol) (eventId :: Symbol). Encodable event => Id convoId -> Id eventId -> event -> Effect Unit
push convoId eventId event = do
  let fileLoc = "./db/" <> unwrap convoId <> "/" <> unwrap eventId <> ".txt"
  writeFile fileLoc (encodeUtf8 <<< encode $ event)

foreign import encodeUtf8 :: String -> Buffer
foreign import decodeUtf8 :: Buffer -> String
