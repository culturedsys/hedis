{-# LANGUAGE OverloadedStrings #-}
module CommandParserSpec(spec) where
import Test.Hspec (describe, it, shouldBe, Spec)
import Resp (Resp(Array, BulkString))
import Command
    ( Command(..), SetArgs(SetArgs), IncrArgs(IncrArgs), SetExArgs (SetExArgs) )
import CommandParser(parse)
import Data.Time (secondsToNominalDiffTime)

spec :: Spec
spec = do
  describe "CommandParser.parse" $ do
    it "can parse a simple set command" $ do
      parse (Array [BulkString "SET", BulkString "key", BulkString "value"]) `shouldBe`
        Right (Set $ SetArgs "key" "value")

    it "can parse an incr command" $ do
      parse (Array [BulkString "INCR", BulkString "key"]) `shouldBe`
        Right (Incr $ IncrArgs "key")

    it "can parse a setnx command" $ do
      parse (Array [BulkString "SETNX", BulkString "key", BulkString "value"]) `shouldBe`
        Right (SetNx $ SetArgs "key" "value")

    it "can parse a setex command" $ do
      parse (Array [BulkString "SETEX", BulkString "key", BulkString "10", BulkString "value"])  `shouldBe`
        Right (SetEx $ SetExArgs "key" (secondsToNominalDiffTime 10) "value")