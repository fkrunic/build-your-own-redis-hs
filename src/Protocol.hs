module Protocol where

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text
import GHC.Generics

data Message
  = Set {key :: Text, value :: Text}
  | Get {key :: Text}
  | Delete {key :: Text}
  deriving (Generic, Eq, Show)

data Response
  = Ok
  | Return {payload :: Text}
  | KeyDoesNotExist {badKey :: Text}
  deriving (Generic, Eq, Show)

instance Binary Message
instance Binary Response

decodeMessage :: BS.ByteString -> Message
decodeMessage = decode . BSL.fromStrict

encodeMessage :: Message -> BS.ByteString
encodeMessage = BS.concat . BSL.toChunks . encode

decodeResponse :: BS.ByteString -> Response
decodeResponse = decode . BSL.fromStrict

encodeResponse :: Response -> BS.ByteString
encodeResponse = BS.concat . BSL.toChunks . encode