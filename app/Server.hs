{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Network.Simple.TCP

handleConnection :: Socket -> IO ()
handleConnection conn = forever $ do
  r <- recv conn 100
  case r of 
    Nothing -> pure ()
    Just msgBytes -> do 
      let msg = T.decodeUtf8 msgBytes
      TIO.putStrLn $ "Client says: " <> msg
      send conn $ T.encodeUtf8 "world!"

main :: IO ()
main = do 
  TIO.putStrLn "Server started."
  serve (Host "0.0.0.0") "1234" $ \(socket, _) -> 
    handleConnection socket
