module Protocol where

import Common
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import GHC.Generics (Generic)

data Response
  = Ok
  | Return {payload :: Text}
  | KeyDoesNotExist {badKey :: Text}
  deriving (Generic, Eq, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

decodeResponse :: BS.ByteString -> Either ParsingError Response
decodeResponse = eitherDecode . BSL.fromStrict

encodeResponse :: Response -> BS.ByteString
encodeResponse = BS.concat . BSL.toChunks . encode