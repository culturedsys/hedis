{-# LANGUAGE OverloadedStrings #-}
module CommandParserSpec(spec) where
import Test.Hspec (describe, it, shouldBe, Spec)
import Resp (Resp(Array, BulkString))
import Command (Command(..))
import CommandParser(parse)
import Command (SetArgs(SetArgs), IncrArgs (IncrArgs))

spec :: Spec
spec = do
  describe "CommandParser.parse" $ do
    it "can parse a simple set command" $ do
      parse (Array [BulkString "SET", BulkString "key", BulkString "value"]) `shouldBe`
        Right (Set $ SetArgs "key" "value")

    it "can parse an incr command" $ do
      parse (Array [BulkString "INCR", BulkString "key"]) `shouldBe`
        Right (Incr $ IncrArgs "key")