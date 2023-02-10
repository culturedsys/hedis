{-# LANGUAGE OverloadedStrings #-}
module CommandProcessor (processCommand, handleCommand) where
import Resp(Resp(..))
import Data.Conduit.Attoparsec (ParseError)
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar, TVar)
import Command (Command (..), SetArgs (SetArgs), GetArgs (GetArgs), IncrArgs (IncrArgs))
import CommandParser (parse)
import AppState(AppState (..))
import qualified Store
import Data.ByteString (ByteString)

processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand s (Right c) = case parse c of
  Right command -> atomically $ handleCommand s command
  Left _ -> return $ Error "ERR Bad command"

handleCommand :: AppState -> Command -> STM Resp
handleCommand (AppState { store = store }) (Set (SetArgs k v)) = handle store (\s -> do
    (_, s') <- Store.set s k v
    Right (SimpleString "OK", s')
  )

handleCommand (AppState { store = store }) (Get (GetArgs k)) = handle store (\s -> do
    (v, s') <- Store.get s k
    Right (maybe NullString BulkString v, s')
  )

handleCommand (AppState { store = store }) (Incr (IncrArgs k)) = handle store (\s -> do
    (result, s') <- Store.incr s k
    Right (Integer result, s')
  )


handle :: TVar Store.Store -> (Store.Store -> Either Store.Error (Resp, Store.Store)) -> STM Resp
handle storeTVar handler = do
  store <- readTVar storeTVar
  case handler store of
    Left err -> return $ Error (messageFor err) 
    Right (result, store') -> do
      writeTVar storeTVar store'
      return result

messageFor :: Store.Error -> ByteString
messageFor Store.BadType = "Bad type"