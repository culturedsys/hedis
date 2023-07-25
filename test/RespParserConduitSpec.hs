{-# LANGUAGE OverloadedStrings #-}

module RespParserConduitSpec (spec) where

import Conduit (runConduitPure, (.|))
import Data.ByteString ()
import Data.Conduit.Combinators (sinkList, yieldMany)
import Data.Either (rights)
import Resp (Resp (..))
import RespParseConduit (respParse, respWrite)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "RespParserConduit.respParse" $ do
    it "can parse a single resp entry" $ do
      let result = runConduitPure (yieldMany ["+STRING\r\n"] .| respParse .| sinkList)
      rights result `shouldBe` [SimpleString "STRING"]
    it "can parse many resp entries" $ do
      let result = runConduitPure (yieldMany ["+STRING\r\n:123\r\n"] .| respParse .| sinkList)
      rights result `shouldBe` [SimpleString "STRING", Integer 123]

  describe "RespParserConduit.respWrite" $ do
    it "can write a simple string" $ do
      let result = runConduitPure (yieldMany [SimpleString "STRING"] .| respWrite .| sinkList)
      mconcat result `shouldBe` "+STRING\r\n"
    it "can write an error" $ do
      let result = runConduitPure (yieldMany [Error "ERROR"] .| respWrite .| sinkList)
      mconcat result `shouldBe` "-ERROR\r\n"
    it "can write an integer" $ do
      let result = runConduitPure (yieldMany [Integer 100] .| respWrite .| sinkList)
      mconcat result `shouldBe` ":100\r\n"
    it "can write a bulk string" $ do
      let result = runConduitPure (yieldMany [BulkString "STRING"] .| respWrite .| sinkList)
      mconcat result `shouldBe` "$6\r\nSTRING\r\n"
    it "can write an array" $ do
      let result = runConduitPure (yieldMany [Array [Integer 1, Integer 2]] .| respWrite .| sinkList)
      mconcat result `shouldBe` "*2\r\n:1\r\n:2\r\n\r\n"
    it "can write a null string" $ do
      let result = runConduitPure (yieldMany [NullString] .| respWrite .| sinkList)
      mconcat result `shouldBe` "$-1\r\n"
