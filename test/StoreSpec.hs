{-# LANGUAGE OverloadedStrings #-}
module StoreSpec (spec) where
import Test.Hspec (Spec, describe, it, shouldBe)
import Store (empty, set, get)

spec :: Spec
spec = do
  describe "Store.Store" $ do
    it "can set and get" $ do
      let store = empty
      let store' = set store "key" "value"
      fst (get store' "key") `shouldBe` Just "value"