{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module CommandProcessor (processCommand, handleCommand) where
import Resp(Resp(..))
import Data.Conduit.Attoparsec (ParseError)
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar, TVar)
import Command (Command (..), SetArgs (SetArgs), SetExArgs (SetExArgs), GetArgs (GetArgs), IncrArgs (IncrArgs), CommandError (CommandParseError, ArgParseError))
import CommandParser (parse)
import AppState(AppState (..))
import qualified Store
import Data.ByteString (ByteString)
import Data.Time (UTCTime, getCurrentTime)


processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand (AppState store) (Right c) = do
  now <- getCurrentTime
  case parse c of
    Right command -> atomically $ handleCommand command now store
    Left CommandParseError -> return $ Error "ERR Bad command"
    Left (ArgParseError input expected) ->
      return . Error $ "ERR Bad argument " <> input <> ": expected " <> expected


handleCommand :: Command -> UTCTime -> TVar Store.Store ->  STM Resp
handleCommand (Set (SetArgs k v)) = handle (Store.set k v) (const $ SimpleString "OK")

handleCommand (Get (GetArgs k)) = handle (Store.get k) (maybe NullString BulkString)

handleCommand (SetNx (SetArgs k v)) =
  handle (Store.setNoOverwrite k v) (\case
    Store.Modified -> SimpleString "OK"
    Store.Unmodified -> NullString
  )

handleCommand (SetEx (SetExArgs k d v)) =
  handle (Store.setWithExpiration k v d) (const (SimpleString "OK"))

handleCommand (Incr (IncrArgs k)) = handle (Store.incr k) Integer


handle ::  Store.StoreM (Store.Result a) -> (a -> Resp) -> UTCTime -> TVar Store.Store -> STM Resp
handle commandAction resultHandler now storeTVar  = do
  store <- readTVar storeTVar
  let (result, store') = Store.runStoreM store now commandAction
  case fmap resultHandler result of
    Left err -> return $ Error (messageFor err)
    Right response -> do
      writeTVar storeTVar store'
      return response


messageFor :: Store.Error -> ByteString
messageFor Store.BadType = "Bad type"