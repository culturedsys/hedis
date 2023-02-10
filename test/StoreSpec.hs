{-# LANGUAGE OverloadedStrings #-}
module StoreSpec (spec) where
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure, Expectation)
import Store (empty, set, get, incr)

spec :: Spec
spec = do
  describe "Store.Store" $ do
    it "can set and get" $ do
      let store = empty
      let setResult = set store "key" "value"
      expectRight setResult
      let Right (_, store') = setResult
      fst <$> get store' "key" `shouldBe` Right (Just "value")

    it "can incr an integer" $ do
      let Right (_, store) = set empty "key" "1"
      let incrResult = incr store "key"
      expectRight incrResult
      fst <$> incrResult `shouldBe` Right 2
      let Right (_, store') = incrResult
      fst <$> get store' "key" `shouldBe` Right (Just "2")


expectRight :: (Show a1) => Either a1 a2 -> Expectation
expectRight actual = 
  case actual of
    Left e -> expectationFailure ("Expected Right, got Left " ++ (show e))
    Right _ -> return ()