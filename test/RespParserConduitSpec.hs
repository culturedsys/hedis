{-# LANGUAGE OverloadedStrings #-}

module RespParserConduitSpec (spec) where
import Test.Hspec (it, describe, shouldBe, Spec)
import Data.Conduit.Combinators (yieldMany, sinkList)
import Conduit ((.|), runConduitPure)
import RespParseConduit (respParse)
import Resp (Resp(..))
import Data.Either (rights)


spec :: Spec
spec = do 
  describe "RespParserConduit.respParser" $ do
    it "can parse a single resp entry" $ do
      let result = runConduitPure $ (yieldMany ["+STRING\r\n"] .| respParse .| sinkList)
      rights result `shouldBe`  [SimpleString "STRING"]
    it "can parse many resp entries" $ do
      let result = runConduitPure $ (yieldMany ["+STRING\r\n:123\r\n"] .| respParse .| sinkList)
      rights result `shouldBe`  [SimpleString "STRING", Integer 123]