{-# LANGUAGE OverloadedStrings #-}
module StoreSpec (spec) where
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure, Expectation)
import Store (empty, set, get, incr, setNoOverwrite, SetResult(..), setWithExpiration)
import Data.Time (UTCTime(UTCTime), fromGregorian, addUTCTime, secondsToNominalDiffTime)

spec :: Spec
spec = do
  let now = UTCTime (fromGregorian 1970 1 1) 0
  describe "Store.Store" $ do
    it "can set and get" $ do
      let store = empty
      let setResult = set store "key" "value" now
      expectRight setResult
      let Right (_, store') = setResult
      fst <$> get store' "key" now `shouldBe` Right (Just "value")

    it "can setNoOverwrite if not present" $ do
      let store = empty
      fst <$> setNoOverwrite store "key" "value" now `shouldBe` Right Modified

    it "cannot setNoOverwrite if present" $ do
      let Right (_, store) = set empty "key" "value" now
      fst <$> setNoOverwrite store "key" "replacement" now `shouldBe` Right Unmodified

    it "can get value setWithExpiry if not after expiry time" $ do
      let duration = secondsToNominalDiffTime 10
      let Right (_, store') = setWithExpiration empty "key" "value" duration now
      fst <$> get store' "key" (addUTCTime (secondsToNominalDiffTime 9) now) `shouldBe` Right (Just "value")

    it "can get nothing setWithExpiry if at expiry time" $ do
      let duration = secondsToNominalDiffTime 10
      let Right (_, store') = setWithExpiration empty "key" "value" duration now
      fst <$> get store' "key" (addUTCTime duration now) `shouldBe` Right Nothing

    it "can incr an integer" $ do
      let Right (_, store) = set empty "key" "1" now
      let incrResult = incr store "key" now
      expectRight incrResult
      fst <$> incrResult `shouldBe` Right 2
      let Right (_, store') = incrResult
      fst <$> get store' "key" now `shouldBe` Right (Just "2")


expectRight :: (Show a1) => Either a1 a2 -> Expectation
expectRight actual =
  case actual of
    Left e -> expectationFailure ("Expected Right, got Left " ++ (show e))
    Right _ -> return ()