{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Simple.TCP 
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

sendMessage :: Socket -> IO ()
sendMessage conn = do
  send conn $ T.encodeUtf8 "hello!"
  r <- recv conn 100 -- reads the first 100 bytes
  case r of 
    Nothing -> TIO.putStrLn "Server sent nothing."
    Just msgBytes -> do 
      let msg = T.decodeUtf8 msgBytes
      TIO.putStrLn $ "Server says: " <> msg  

main :: IO ()
main = do
  TIO.putStrLn "Client started."
  connect "0.0.0.0" "1234" $ \(conn, _) -> do
    sendMessage conn
    sendMessage conn
    sendMessage conn
