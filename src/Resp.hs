module Resp (Resp (..)) where

import Data.ByteString (ByteString)

data Resp
  = SimpleString ByteString
  | Error ByteString
  | Integer Int
  | BulkString ByteString
  | Array [Resp]
  | NullString
  deriving (Show, Eq)