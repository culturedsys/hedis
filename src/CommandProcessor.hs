{-# LANGUAGE OverloadedStrings #-}
module CommandProcessor (processCommand, handleCommand) where
import Resp(Resp(..))
import Data.Conduit.Attoparsec (ParseError)
import Control.Concurrent.STM (STM, atomically, modifyTVar', readTVar)
import Command (Command (..))
import CommandParser (parse)
import AppState(AppState (..))
import qualified Store

processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand s (Right c) = case parse c of
  Right command -> atomically $ handleCommand s command
  Left _ -> return $ Error "ERR Bad command"

handleCommand :: AppState -> Command -> STM Resp
handleCommand (AppState { store = store }) (Set { cKey = k, csValue = v}) = do
  modifyTVar' store (\s -> Store.set s k v)
  return $ SimpleString "OK"
handleCommand (AppState { store = store }) (Get { cKey = k }) = do
  s <- readTVar store
  return $ maybe NullString BulkString (fst $ Store.get s k) 
