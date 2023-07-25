{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandProcessor (processCommand, handleCommand) where

import AppState (AppState (..))
import Command
  ( Command (..),
    CommandError (ArgParseError, CommandParseError),
    DecrArgs (DecrArgs),
    GetArgs (GetArgs),
    IncrArgs (IncrArgs),
    SetArgs (SetArgs),
    SetExArgs (SetExArgs),
  )
import CommandParser (parse)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec (ParseError)
import Data.Time (UTCTime, getCurrentTime)
import Resp (Resp (..))
import Store qualified

processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand (AppState store) (Right c) = do
  now <- getCurrentTime
  case parse c of
    Right command -> atomically $ handleCommand command now store
    Left CommandParseError -> return $ Error "ERR Bad command"
    Left (ArgParseError input expected) ->
      return . Error $ "ERR Bad argument " <> input <> ": expected " <> expected

handleCommand :: Command -> UTCTime -> TVar Store.Store -> STM Resp
handleCommand (Set (SetArgs k v)) = handle (Store.set k v) (const $ SimpleString "OK")
handleCommand (Get (GetArgs k)) = handle (Store.get k) (maybe NullString BulkString)
handleCommand (SetNx (SetArgs k v)) =
  handle
    (Store.setNoOverwrite k v)
    ( \case
        Store.Modified -> SimpleString "OK"
        Store.Unmodified -> NullString
    )
handleCommand (SetEx (SetExArgs k d v)) =
  handle (Store.setWithExpiration k v d) (const (SimpleString "OK"))
handleCommand (Incr (IncrArgs k)) = handle (Store.incr k) Integer
handleCommand (Decr (DecrArgs k)) = handle (Store.decr k) Integer

handle :: Store.StoreM (Store.Result a) -> (a -> Resp) -> UTCTime -> TVar Store.Store -> STM Resp
handle commandAction resultHandler now storeTVar = do
  store <- readTVar storeTVar
  let (result, store') = Store.runStoreM store now commandAction
  case fmap resultHandler result of
    Left err -> return $ Error (messageFor err)
    Right response -> do
      writeTVar storeTVar store'
      return response

messageFor :: Store.Error -> ByteString
messageFor Store.BadType = "Bad type"