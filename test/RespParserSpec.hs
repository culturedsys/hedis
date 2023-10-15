{-# LANGUAGE OverloadedStrings #-}

module RespParserSpec (spec) where

import Data.Attoparsec.ByteString (parseOnly, endOfInput, Parser)
import Resp (Resp (..))
import RespParser (respParser)
import Test.Hspec (Spec, describe, it, shouldBe)
import Data.ByteString (ByteString)

parseAll :: Parser a -> ByteString -> Either String a 
parseAll parser = parseOnly (parser <* endOfInput)

spec :: Spec
spec = do
  describe "RespParser.parse" $ do
    it "can parse a simple string" $ do
      parseAll respParser "+VALUE\r\n" `shouldBe` Right (SimpleString "VALUE")
    it "can parse an error" $ do
      parseAll respParser "-ERR\r\n" `shouldBe` Right (Error "ERR")
    it "can parse an integer" $ do
      parseOnly respParser ":10\r\n" `shouldBe` Right (Integer 10)
    it "can parse a bulk string" $ do
      parseAll respParser "$5\r\nhello\r\n" `shouldBe` Right (BulkString "hello")
    it "can parse a null string" $ do
      parseAll respParser "$-1\r\n" `shouldBe` Right NullString
    it "can parse an array" $ do
      parseOnly respParser "*3\r\n:1\r\n:2\r\n:3\r\n" `shouldBe` Right (Array [Integer 1, Integer 2, Integer 3])
    it "can parse a nested array" $ do
      parseAll respParser "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Hello\r\n-World\r\n"
        `shouldBe` Right (Array [Array [Integer 1, Integer 2, Integer 3], Array [SimpleString "Hello", Error "World"]])
    it "can parse a null string" $ do
      parseAll respParser "$-1\r\n" `shouldBe` Right NullString
    it "can parse a command" $ do
      parseAll respParser "*2\r\n$3\r\nGET\r\n$3\r\nfoo\r\n"
        `shouldBe` Right (Array [BulkString "GET", BulkString "foo"])