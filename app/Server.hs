{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Map.Strict
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Simple.TCP
import Storage.Strings
import Protocol
import Prelude hiding (lookup)

newtype Database = Database
  { stringStore :: TVar StringStore
  }

initialize :: IO Database
initialize = do
  stringStore <- newTVarIO empty
  let db = Database{stringStore}
  pure db

handleConnection :: Socket -> TVar StringStore -> IO ()
handleConnection conn ref = forever $ do
  r <- recv conn 100 -- reads the first 100 bytes
  case r of
    Nothing -> pure ()
    Just msgBytes -> do
      case decodeCommand msgBytes of
        Left err -> TIO.putStrLn $ "Command parsing failure: " <> T.pack err
        Right cmd -> do
          TIO.putStrLn $ "Received command: " <> T.pack (show cmd)
          response <- atomically (processCommand cmd ref)
          send conn (encodeResponse response)

main :: IO ()
main = do
  TIO.putStrLn "Server started."
  db <- initialize
  serve (Host "0.0.0.0") "1234" $ \(socket, _) ->
    handleConnection socket (stringStore db)
