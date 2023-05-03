module Storage.Strings where

import Common
import Control.Concurrent.STM
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (lookup)
import Protocol

data Command
  = Set {key :: Text, value :: Text}
  | Get {key :: Text}
  | Delete {key :: Text}
  deriving (Generic, Eq, Show)

type StringStore = Map Text Text

processCommand :: Command -> TVar StringStore -> STM Response
processCommand (Set{key, value}) ref = do
  ds <- readTVar ref
  let updated = insertWith const key value ds
  writeTVar ref updated
  pure Ok
processCommand (Get{key}) ref = do
  ds <- readTVar ref
  case lookup key ds of
    Nothing -> pure (KeyDoesNotExist{badKey = key})
    Just payload -> pure (Return{payload})
processCommand (Delete{key}) ref = do
  ds <- readTVar ref
  let updated = delete key ds
  writeTVar ref updated
  pure Ok  

instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Command

decodeCommand :: BS.ByteString -> Either ParsingError Command
decodeCommand = eitherDecode . BSL.fromStrict

encodeCommand :: Command -> BS.ByteString
encodeCommand = BS.concat . BSL.toChunks . encode