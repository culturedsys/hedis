{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RespParser (parser) where
import Data.Attoparsec.ByteString as P (Parser, word8, takeTill, string, satisfy, many1, take, count)
import Data.Word8
import Resp
import Control.Applicative ((<|>), optional)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)

crlf :: Parser ByteString
crlf = string "\r\n"

crlfTerminated :: Parser ByteString
crlfTerminated = takeTill (== _cr) <* crlf

number :: Parser Int
number = do
  sign <- optional (word8 _hyphen)
  let m = if isJust sign then -1 else 1
  (m *) . word8ArrayToInt <$> many1 (satisfy isDigit)
  where word8ArrayToInt = foldl (\acc v -> acc * 10 + fromIntegral (v - _0)) 0

parser :: Parser Resp
parser = 
  (word8 _plus *> (SimpleString <$> crlfTerminated))
  <|> (word8 _hyphen *> (Error <$> crlfTerminated))
  <|> (word8 _colon *> (Integer <$> number <* crlf))
  <|> (word8 _dollar *> (do
      len <- number <* crlf
      if len == -1 then
        return NullString
      else
        BulkString <$> P.take len <* crlf
    ))
  <|> (word8 _asterisk *> (Array <$> do
      len <- number <* crlf
      count len parser
    ))