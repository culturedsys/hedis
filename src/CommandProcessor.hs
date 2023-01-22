{-# LANGUAGE OverloadedStrings #-}
module CommandProcessor (processCommand, handleCommand) where
import Resp(Resp(..))
import Data.Conduit.Attoparsec (ParseError)
import qualified Data.Map as M
import Control.Concurrent.STM (STM, atomically, modifyTVar')
import Command (Command (..))
import CommandParser (parse)
import AppState(AppState (..))

processCommand :: AppState -> Either ParseError Resp -> IO Resp
processCommand _ (Left _) = return $ Error "ERR Parse error"
processCommand s (Right c) = case parse c of
  Right command -> atomically $ handleCommand s command
  Left _ -> return $ Error "ERR Bad command"

handleCommand :: AppState -> Command -> STM Resp
handleCommand (AppState { store = s }) (Set { csKey = k, csValue = v}) = do
  modifyTVar' s (M.insert k v)
  return $ SimpleString "OK"