module Storage.Strings where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import GHC.Generics (Generic)

data Command
  = Set {key :: Text, value :: Text}
  | Get {key :: Text}
  | Delete {key :: Text}
  deriving (Generic, Eq, Show)

data Response
  = Ok
  | Return {payload :: Text}
  | KeyDoesNotExist {badKey :: Text}
  deriving (Generic, Eq, Show)

instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Command
instance FromJSON Response

type ParsingError = String

decodeCommand :: BS.ByteString -> Either ParsingError Command
decodeCommand = eitherDecode . BSL.fromStrict

encodeCommand :: Command -> BS.ByteString
encodeCommand = BS.concat . BSL.toChunks . encode

decodeResponse :: BS.ByteString -> Either ParsingError Response
decodeResponse = eitherDecode . BSL.fromStrict

encodeResponse :: Response -> BS.ByteString
encodeResponse = BS.concat . BSL.toChunks . encode