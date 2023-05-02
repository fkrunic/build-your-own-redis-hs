{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Simple.TCP 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Protocol

sendMessage :: Socket -> Message -> IO ()
sendMessage conn msg = do
  send conn (encodeMessage msg)
  r <- recv conn 100 -- reads the first 100 bytes
  case r of 
    Nothing -> TIO.putStrLn "Server sent nothing."
    Just msgBytes -> do 
      let serverResponse = decodeResponse msgBytes
      TIO.putStrLn $ "Server says: " <> T.pack (show serverResponse)

main :: IO ()
main = do
  TIO.putStrLn "Client started."
  connect "0.0.0.0" "1234" $ \(conn, _) -> do
    sendMessage conn $ Set { key = "dog", value = "steven" }
    sendMessage conn $ Get { key = "cat" }
    sendMessage conn $ Delete { key = "filip" }
