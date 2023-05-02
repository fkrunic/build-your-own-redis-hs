{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.Simple.TCP
import Protocol

handleConnection :: Socket -> IO ()
handleConnection conn = forever $ do
  r <- recv conn 100 -- reads the first 100 bytes
  case r of 
    Nothing -> pure ()
    Just msgBytes -> do 
      let msg = decodeMessage msgBytes
      TIO.putStrLn $ "Client says: " <> T.pack (show msg)
      send conn (encodeResponse Ok)

main :: IO ()
main = do 
  TIO.putStrLn "Server started."
  serve (Host "0.0.0.0") "1234" $ \(socket, _) -> 
    handleConnection socket
