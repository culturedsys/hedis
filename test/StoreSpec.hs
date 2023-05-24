{-# LANGUAGE OverloadedStrings #-}
module StoreSpec (spec) where
import Test.Hspec (Spec, describe, it, shouldBe)
import Store (empty, set, get, incr, setNoOverwrite, SetResult(..), setWithExpiration, runStoreM)
import Data.Time (UTCTime(UTCTime), fromGregorian, addUTCTime, secondsToNominalDiffTime)

spec :: Spec
spec = do
  let now = UTCTime (fromGregorian 1970 1 1) 0
  describe "Store.Store" $ do
    it "can set and get" $ do
      let store = empty
      let (result, _) = runStoreM store now $ do
            set "key" "value"
            get "key"
      result `shouldBe` Right (Just "value")

    it "can setNoOverwrite if not present" $ do
      let store = empty
      let (result, _) = runStoreM store now (setNoOverwrite "key" "value")
      result `shouldBe` Right Modified

    it "cannot setNoOverwrite if present" $ do
      let (result, _) = runStoreM empty now $ do
            set "key" "value"
            setNoOverwrite "key" "replacement"
      result `shouldBe` Right Unmodified

    it "can get value setWithExpiry if not after expiry time" $ do
      let duration = secondsToNominalDiffTime 10
      let (_, store) = runStoreM empty now (setWithExpiration "key" "value" duration)
      let (result, _) = runStoreM store (addUTCTime (secondsToNominalDiffTime 9) now) (get "key")
      result `shouldBe` Right (Just "value")

    it "can get nothing setWithExpiry if at expiry time" $ do
      let duration = secondsToNominalDiffTime 10
      let (_, store) = runStoreM empty now (setWithExpiration "key" "value" duration)
      let (result, _) = runStoreM store (addUTCTime duration now) (get "key")  
      result `shouldBe` Right Nothing

    it "can incr an integer" $ do
      let (result, store) = runStoreM empty now $ do 
            set "key" "1"
            incr "key"
      result `shouldBe` Right 2
      let (result', _) = runStoreM store now (get "key")
      result' `shouldBe` Right (Just "2")
