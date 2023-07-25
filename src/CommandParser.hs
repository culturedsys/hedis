{-# LANGUAGE OverloadedStrings #-}

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

parse :: Resp -> Either CommandError Command
parse (Array [BulkString "SET", BulkString k, BulkString v]) = Right . Set $ SetArgs k v
parse (Array [BulkString "GET", BulkString k]) = Right . Get $ GetArgs k
parse (Array [BulkString "SETNX", BulkString k, BulkString v]) = Right . SetNx $ SetArgs k v
parse (Array [BulkString "SETEX", BulkString k, BulkString d, BulkString v]) = case byteStringToInt d of
  Nothing -> Left $ ArgParseError d "number"
  Just i ->
    let duration = secondsToNominalDiffTime $ fromIntegral i
     in Right . SetEx $ SetExArgs k duration v
parse (Array [BulkString "INCR", BulkString k]) = Right . Incr $ IncrArgs k
parse (Array [BulkString "DECR", BulkString k]) = Right . Decr $ DecrArgs k
parse _ = Left CommandParseError