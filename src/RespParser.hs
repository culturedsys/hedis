{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RespParser (respParser) where

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
crlfTerminated = takeTill (== _cr)

magnitudeParser :: Parser Int
magnitudeParser = fromJust . word8ArrayToInt <$> many1 (satisfy isDigit)

numberParser :: Parser Int
numberParser = do
  sign <- optional (word8 _hyphen)
  magnitude <- magnitudeParser
  return $ if isJust sign then -magnitude else magnitude

valueParser :: (a -> Resp) -> Word8 -> Parser a -> Parser Resp
valueParser wrap sigil p = wrap <$> (word8 sigil *> p)

simpleStringParser :: Parser Resp
simpleStringParser = valueParser SimpleString _plus crlfTerminated <* crlf

errorParser :: Parser Resp
errorParser = valueParser Error _hyphen crlfTerminated <* crlf

integerParser :: Parser Resp
integerParser = valueParser Integer _colon numberParser <* crlf

bulkStringParser :: Parser Resp
bulkStringParser =
  valueParser
    BulkString
    _dollar
    ( do
        len <- magnitudeParser <* crlf
        P.take len <* crlf
    )

nullStringParser :: Parser Resp
nullStringParser =
  valueParser
    (const NullString)
    _dollar
    (word8 _hyphen >> word8 _1 >> crlf)

arrayParser :: Parser Resp
arrayParser =
  valueParser
    Array
    _asterisk
    ( do
        len <- magnitudeParser <* crlf
        count len respParser
    )

respParser :: Parser Resp
respParser =
  simpleStringParser
    <|> errorParser
    <|> integerParser
    <|> bulkStringParser
    <|> nullStringParser
    <|> arrayParser