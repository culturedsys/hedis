{-# LANGUAGE OverloadedStrings #-}
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
handleCommand (Set (SetArgs k v)) = handle  (\now s -> do
    (_, s') <- Store.set s k v now
    Right (SimpleString "OK", s')
  )

handleCommand (Get (GetArgs k)) = handle (\now s -> do
    (v, s') <- Store.get s k now
    Right (maybe NullString BulkString v, s')
  )

handleCommand (SetNx (SetArgs k v)) = handle (\now s -> do
    (result, s') <- Store.setNoOverwrite s k v now
    case result of
      Store.Modified -> Right (SimpleString "OK", s')
      Store.Unmodified -> Right (NullString, s')
  )

handleCommand (SetEx (SetExArgs k d v)) = handle (\now s -> do
    (_, s') <- Store.setWithExpiration s k v d now
    Right (SimpleString "OK", s')
  )

handleCommand (Incr (IncrArgs k)) = handle (\now s -> do
    (result, s') <- Store.incr s k now
    Right (Integer result, s')
  )


handle ::  (UTCTime -> Store.Store -> Either Store.Error (Resp, Store.Store)) -> UTCTime -> TVar Store.Store -> STM Resp
handle handler now storeTVar  = do
  store <- readTVar storeTVar
  case handler now store of
    Left err -> return $ Error (messageFor err) 
    Right (result, store') -> do
      writeTVar storeTVar store'
      return result

messageFor :: Store.Error -> ByteString
messageFor Store.BadType = "Bad type"