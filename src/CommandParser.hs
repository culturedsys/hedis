{-# LANGUAGE OverloadedStrings #-}
module CommandParser(parse) where
import Resp(Resp(..))
import Command(Command(..), CommandError(..))


parse :: Resp -> Either CommandError Command
parse (Array (BulkString "SET" : BulkString k : BulkString v : _)) = Right $ Set k v
parse (Array (BulkString "GET" : BulkString k : [])) = Right $ Get k
parse _ = Left CommandParseError