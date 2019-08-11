module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
    hSetBuffering,
    stdout)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed bad) = (intersperse ' ' $ 
        fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar x = if isJust x then fromJust x else '_' 

freshPuzzle :: String -> Puzzle
freshPuzzle hidden = Puzzle hidden p [] []
    where p = fmap (const Nothing) hidden

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) x = elem x word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) x = elem x guessed

fillInCharacter :: Puzzle -> Char -> Maybe Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s b) c nb= 
    Puzzle word newFilledInSoFar (c:s) newb
    where 
        zipper guessed wordChar guessChar = if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        newb = if nb == Nothing then b else b++[fromJust nb]
        
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle _ _ _ b) guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You alread guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word \
                \accordingly"
            return (fillInCharacter puzzle guess Nothing)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return (fillInCharacter puzzle guess (Just guess))

gameOver :: Puzzle -> IO()
gameOver (Puzzle wordToGuess _ guessed bad) = 
    if (length bad) > 7 then 
        do 
            putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
        else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) = 
    if all isJust filledInSoFar then
        do 
            putStrLn "You win!"
            exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of [c] -> handleGuess puzzle c >>= runGame
                  _ -> putStrLn "Your guess must be a single character"

allWords :: IO WordList
allWords = do 
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w = 
            let l = length (w :: String)
            in      l >= minWordLength
                && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0 , (length wl) - 1 )
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
