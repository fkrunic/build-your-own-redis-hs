module Protocol where

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text
import GHC.Generics

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

instance Binary Command
instance Binary Response

decodeCommand :: BS.ByteString -> Command
decodeCommand = decode . BSL.fromStrict

encodeCommand :: Command -> BS.ByteString
encodeCommand = BS.concat . BSL.toChunks . encode

decodeResponse :: BS.ByteString -> Response
decodeResponse = decode . BSL.fromStrict

encodeResponse :: Response -> BS.ByteString
encodeResponse = BS.concat . BSL.toChunks . encode