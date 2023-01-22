{-# LANGUAGE OverloadedStrings #-}

module RespParserConduitSpec (spec) where
import Test.Hspec (it, describe, shouldBe, Spec)
import Data.Conduit.Combinators (yieldMany, sinkList)
import Conduit ((.|), runConduitPure)
import RespParseConduit (respParse, respWrite)
import Resp (Resp(..))
import Data.Either (rights)
import Data.ByteString()

spec :: Spec
spec = do 
  describe "RespParserConduit.respParse" $ do
    it "can parse a single resp entry" $ do
      let result = runConduitPure (yieldMany ["+STRING\r\n"] .| respParse .| sinkList)
      rights result `shouldBe`  [SimpleString "STRING"]
    it "can parse many resp entries" $ do
      let result = runConduitPure (yieldMany ["+STRING\r\n:123\r\n"] .| respParse .| sinkList)
      rights result `shouldBe`  [SimpleString "STRING", Integer 123]

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
