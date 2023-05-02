module Protocol where

import Data.Binary
import Data.Text
import GHC.Generics

data Message 
  = Set { key :: Text, value :: Text }
  | Get { key :: Text }
  | Delete { key :: Text }
  deriving (Generic, Eq)

instance Binary Message