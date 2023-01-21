{-# LANGUAGE OverloadedStrings #-}
module RespParser (parser) where
import Data.Attoparsec.ByteString as P (Parser, word8, takeTill, string, satisfy, many1, take, count)
import Data.Word8
import Resp
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)

crlf :: Parser ByteString
crlf = string "\r\n"

crlfTerminated :: Parser ByteString
crlfTerminated = takeTill (== _cr) <* crlf

number :: Parser Int
number = word8ArrayToInt <$> many1 (satisfy isDigit)
  where word8ArrayToInt = foldl (\acc v -> acc * 10 + fromIntegral (v - _0)) 0

parser :: Parser Resp
parser = 
  (word8 _plus *> (SimpleString <$> crlfTerminated))
  <|> (word8 _hyphen *> (Error <$> crlfTerminated))
  <|> (word8 _colon *> (Integer <$> number <* crlf))
  <|> (word8 _dollar *> (BulkString <$> do
      len <- number <* crlf
      P.take len <* crlf
    ))
  <|> (word8 _asterisk *> (Array <$> do
      len <- number <* crlf
      count len parser
    ))