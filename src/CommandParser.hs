{-# LANGUAGE OverloadedStrings #-}
module CommandParser(parse) where
import Resp(Resp(..))
import Command(Command(..), CommandError(..))


parse :: Resp -> Either CommandError Command
parse (Array (BulkString "SET" : BulkString k : BulkString v : _)) = Right $ Set k v
parse _ = Left CommandParseError