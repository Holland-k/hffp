module Main where 

import qualified Data.Map as M
import Test.QuickCheck
import Test.Hspec
import Hangman

allowedChars :: [Char]
allowedChars = ['a'..'z']

charGen :: Gen Char
charGen = elements allowedChars


main :: IO ()
main = hspec $ do
    describe "Character" $ do
        it "\'a\' should not be in word \'hello\'" $ do
            fillInCharacter (Puzzle "hello" [Nothing] "bc" "c") 'a' (Just 'c') 
                `shouldBe` (Puzzle "hello" [Nothing] "abc" "ca")
        it "\'e\' should be in word \'hello\'" $ do
            fillInCharacter (Puzzle "hello" [Nothing,Nothing,Nothing,Nothing,Nothing] "bc" "c") 'e' Nothing
                `shouldBe` (Puzzle "hello" [Nothing,Just 'e',Nothing,Nothing,Nothing] "ebc" "c")
    describe "Letter 'e' is" $ do
        it "a letter in 'hello'" $ do 
            (handleGuess (freshPuzzle "hello") 'e')
                `shouldBe` (Puzzle "hello" [Nothing,Just 'e',Nothing,Nothing,Nothing] "e" "e")