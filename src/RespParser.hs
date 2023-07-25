{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RespParser (parser) where

import BsUtil (word8ArrayToInt)
import Control.Applicative (optional, (<|>))
import Data.Attoparsec.ByteString as P (Parser, count, many1, satisfy, string, take, takeTill, word8)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust, isJust)
import Data.Word8
import Resp (Resp (..))

crlf :: Parser ByteString
crlf = string "\r\n"

crlfTerminated :: Parser ByteString
crlfTerminated = takeTill (== _cr) <* crlf

number :: Parser Int
number = do
  sign <- optional (word8 _hyphen)
  let m = if isJust sign then -1 else 1
  (m *) . fromJust . word8ArrayToInt <$> many1 (satisfy isDigit)

parser :: Parser Resp
parser =
  (word8 _plus *> (SimpleString <$> crlfTerminated))
    <|> (word8 _hyphen *> (Error <$> crlfTerminated))
    <|> (word8 _colon *> (Integer <$> number <* crlf))
    <|> ( word8 _dollar
            *> ( do
                   len <- number <* crlf
                   if len == -1
                     then return NullString
                     else BulkString <$> P.take len <* crlf
               )
        )
    <|> ( word8 _asterisk
            *> ( Array <$> do
                   len <- number <* crlf
                   count len parser
               )
        )