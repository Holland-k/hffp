module Addition where

import Test.Hspec
import Test.QuickCheck

mmul :: (Eq a, Num a) => a -> a -> a
mmul b 0 = 0
mmul b c = b + mmul b (c-1)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1+1 is greater than 1" $ do 
            (1+1) > 1 `shouldBe` True
        it "2+2 is equal to 4" $ do
            2+2 `shouldBe` 4
        it "5*4 is equal to 20" $ do
            mmul 5 4 `shouldBe` 20
        it "5 *1 is equal to 5" $ do
            mmul 5 1 `shouldBe` 5
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)