{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CommandParser (parse) where

import BsUtil (byteStringToInt)
import Command
  ( Command (..),
    CommandError (..),
    DecrArgs (DecrArgs),
    GetArgs (..),
    IncrArgs (..),
    SetArgs (..),
    SetExArgs (SetExArgs),
  )
import Data.Time (secondsToNominalDiffTime)
import Resp (Resp (..))
import Data.ByteString (ByteString)

extract :: Resp -> Maybe [ByteString] 
extract (Array parts) = traverse (\case BulkString s -> Just s; _ -> Nothing) parts
extract _ = Nothing

parse :: Resp -> Either CommandError Command
parse resp = case extract resp of
  Just ["SET", k, v] -> Right . Set $ SetArgs k v
  Just ["GET", k] -> Right . Get $ GetArgs k
  Just ["SETNX", k, v] -> Right . SetNx $ SetArgs k v
  Just ["SETEX", k, d, v] -> case byteStringToInt d of
    Nothing -> Left $ ArgParseError d "number"
    Just i ->
      let duration = secondsToNominalDiffTime $ fromIntegral i
      in Right . SetEx $ SetExArgs k duration v
  Just ["INCR", k] -> Right . Incr $ IncrArgs k
  Just ["DECR", k] -> Right . Decr $ DecrArgs k
  _ -> Left CommandParseError