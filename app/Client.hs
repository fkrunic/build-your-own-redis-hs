{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Simple.TCP
import Protocol
import Storage.Strings

sendMessage :: Socket -> Command -> IO ()
sendMessage conn cmd = do
  send conn (encodeCommand cmd)
  r <- recv conn 100 -- reads the first 100 bytes
  case r of
    Nothing -> TIO.putStrLn "Server sent no reply."
    Just msgBytes -> do
      case decodeResponse msgBytes of
        Left err -> TIO.putStrLn $ "Response parsing failure: " <> T.pack err
        Right response -> TIO.putStrLn $ T.pack (show response)

main :: IO ()
main = do
  TIO.putStrLn "Client started."
  connect "0.0.0.0" "1234" $ \(conn, _) -> do
    sendMessage conn $ Set{key = "dog", value = "steven"}
    sendMessage conn $ Get{key = "cat"}
    sendMessage conn $ Delete{key = "filip"}
    sendMessage conn $ Get{key = "dog"}
