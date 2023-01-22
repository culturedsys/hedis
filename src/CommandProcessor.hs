{-# LANGUAGE OverloadedStrings #-}
module CommandProcessor (processCommand, handleCommand) where
import Resp(Resp(..))
import Data.Conduit.Attoparsec (ParseError)
import qualified Data.Map as M
import Control.Concurrent.STM (STM, atomically, modifyTVar', readTVar)
import Command (Command (..))
import CommandParser (parse)
import AppState(AppState (..))

processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand s (Right c) = case parse c of
  Right command -> atomically $ handleCommand s command
  Left _ -> return $ Error "ERR Bad command"

handleCommand :: AppState -> Command -> STM Resp
handleCommand (AppState { store = s }) (Set { cKey = k, csValue = v}) = do
  modifyTVar' s (M.insert k v)
  return $ SimpleString "OK"
handleCommand (AppState { store = s}) (Get { cKey = k }) = do
  m <- readTVar s
  return $ maybe NullString BulkString (M.lookup k m) 
