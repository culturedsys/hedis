{-# LANGUAGE OverloadedStrings #-}
module CommandParserSpec(spec) where
import Test.Hspec (describe, it, shouldBe, Spec)
import Resp (Resp(Array, BulkString))
import Command (Command(Set))
import CommandParser(parse)

spec :: Spec
spec = do
  describe "CommandParser.parse" $ do
    it "can parse a simple set command" $ do
      parse (Array [BulkString "SET", BulkString "key", BulkString "value"]) `shouldBe`
        Right (Set "key" "value")