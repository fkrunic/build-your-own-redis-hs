module Protocol where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Binary
import Data.Text
import GHC.Generics

data Message 
  = Set { key :: Text, value :: Text }
  | Get { key :: Text }
  | Delete { key :: Text }
  deriving (Generic, Eq, Show)

data Response
  = Ok
  | Error
  | KeyDoesNotExist { badKey :: Text }
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