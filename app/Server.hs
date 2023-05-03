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

type StringStore = Map T.Text T.Text

initialize :: IO Database
initialize = do
  stringStore <- newTVarIO empty
  let db = Database{stringStore}
  pure db

processCommand :: Command -> TVar StringStore -> STM Response
processCommand (Set{key, value}) ref = do
  ds <- readTVar ref
  let updated = insertWith const key value ds
  writeTVar ref updated
  pure Ok
processCommand (Get{key}) ref = do
  ds <- readTVar ref
  case lookup key ds of
    Nothing -> pure (KeyDoesNotExist{badKey = key})
    Just payload -> pure (Return{payload})
processCommand (Delete{key}) ref = do
  ds <- readTVar ref
  let updated = delete key ds
  writeTVar ref updated
  pure Ok

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
