module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

data Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a)  where
    arbitrary = identityGen

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do 
    a <- arbitrary
    return (Identity a)

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenChar :: Gen (Identity Char)
identityGenChar = identityGen

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do 
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a,
        return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do 
    a <- arbitrary
    b <- arbitrary
    frequency [(10, return $ First a),
                (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

main :: IO ()
main = do 
    --sample trivialGen
    --sample identityGenInt
    --sample identityGenChar
    --sample sumGenCharInt
    sample sumGenCharIntFirst
