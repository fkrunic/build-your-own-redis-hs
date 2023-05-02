{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Simple.TCP 
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  TIO.putStrLn "Client started."
  connect "0.0.0.0" "1234" $ \(conn, _) -> do
    send conn $ T.encodeUtf8 "hello!"
    r <- recv conn 100
    case r of 
      Nothing -> TIO.putStrLn "Server sent nothing."
      Just msgBytes -> do 
        let msg = T.decodeUtf8 msgBytes
        TIO.putStrLn $ "Server says: " <> msg
