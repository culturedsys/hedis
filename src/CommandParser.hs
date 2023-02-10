{-# LANGUAGE OverloadedStrings #-}
module CommandParser(parse) where
import Resp(Resp(..))
import Command(Command(..), CommandError(..), SetArgs(..), GetArgs(..), IncrArgs(..))


parse :: Resp -> Either CommandError Command
parse (Array [BulkString "SET", BulkString k, BulkString v]) = Right . Set $ SetArgs k v
parse (Array [BulkString "GET", BulkString k]) = Right . Get $ GetArgs k
parse (Array [BulkString "INCR", BulkString k]) = Right . Incr $ IncrArgs k
parse _ = Left CommandParseError