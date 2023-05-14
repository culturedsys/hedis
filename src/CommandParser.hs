{-# LANGUAGE OverloadedStrings #-}
module CommandParser(parse) where
import Resp(Resp(..))
import Command(Command(..), CommandError(..), SetArgs(..), GetArgs(..), IncrArgs(..), SetExArgs (SetExArgs))
import BsUtil (byteStringToInt)
import Data.Time (secondsToNominalDiffTime)


parse :: Resp -> Either CommandError Command
parse (Array [BulkString "SET", BulkString k, BulkString v]) = Right . Set $ SetArgs k v
parse (Array [BulkString "GET", BulkString k]) = Right . Get $ GetArgs k
parse (Array [BulkString "SETNX", BulkString k, BulkString v]) = Right . SetNx $ SetArgs k v
parse (Array [BulkString "SETEX", BulkString k, BulkString d, BulkString v]) = case byteStringToInt d of
    Nothing -> Left $ ArgParseError d "number"
    Just i -> let duration = secondsToNominalDiffTime $ fromIntegral i in
      Right . SetEx $ SetExArgs k duration v
parse (Array [BulkString "INCR", BulkString k]) = Right . Incr $ IncrArgs k
parse _ = Left CommandParseError