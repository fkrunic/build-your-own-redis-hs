{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Simple.TCP
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = serve (Host "0.0.0.0") "1234" $ \(socket, _) -> do
  r <- recv socket 100
  TIO.putStrLn "Server started."
  case r of 
    Nothing -> TIO.putStrLn "Client sent nothing."
    Just msgBytes -> do 
      let msg = T.decodeUtf8 msgBytes
      TIO.putStrLn $ "Client sent: " <> msg
      send socket $ T.encodeUtf8 "world!"