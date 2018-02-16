module Platy.UtilsSpec where


import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck


import Platy.Datatypes
import Platy.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findDuplicate" $ do
    it "empty list" $ do
      let l1     = [] :: [Int]
          actual = findDuplicate l1
          expect = Nothing
      actual `shouldBe` expect

    it "duplicated list" $ do
      let l1     = [1, 2, 3, 4, 5, 2]
          actual = findDuplicate l1
          expect = Just 2
      actual `shouldBe` expect

    it "not-duplicated list" $ do
      let l1     = [3, 0, 9, 1, 8, 2]
          actual = findDuplicate l1
          expect = Nothing
      actual `shouldBe` expect