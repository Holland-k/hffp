module Hffp where

import Data.Maybe

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two c d) = (a == c) && (b == d)

data StringOrInt = 
    TisAnInt Int 
    | TisAString String

instance Eq StringOrInt where 
    (==) (TisAnInt a) (TisAnInt b) = a == b
    (==) (TisAString a) (TisAString b) = a == b
    (==) _ _ = False

data Pair a = Pair a a deriving (Show)

instance Eq a => Eq (Pair a) where
    (==) (Pair b b') (Pair c c') = b == c && b' == c'

data Tuple a b = Tuple a b deriving (Show)

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple c d) (Tuple c' d') = (c == c') && (d == d')

data Which a = 
    ThisOne a
    | ThatOne a deriving (Show)

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne b) (ThisOne c) = b == c
    (==) (ThatOne b) (ThatOne c) = b == c
    (==) _ _ = False

data EitherOr a b = 
    Hello a
    | Goodbye b deriving (Show)

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello c) (Hello c') = (c == c')
    (==) (Goodbye c) (Goodbye c') = (c == c')
    (==) _ _ = False

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Ord)

instance Eq DayOfWeek where 
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "okay" else error "Failed"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right
 
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = if preorder testTree == [2,1,3]
    then putStrLn "Okay"
    else putStrLn "Failed"

testInorder :: IO()
testInorder = if inorder testTree == [1,2,3]
    then putStrLn "Okay"
    else putStrLn "Failed"

testPostorder :: IO()
testPostorder = if postorder testTree == [1, 3, 2]
    then putStrLn "Okay"
    else putStrLn "Failed"

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x Leaf = x
foldTree f x (Node left y right) = 
    f y (foldTree f (foldTree f x left) right)

testFold = if foldTree (+) 0 testTree == 6
    then print "okay"
    else print "failed"

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xss) ys@(y:yss) = 
    if x == y 
        then isSubseqOf xss yss
        else isSubseqOf xs yss

data Expr = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Add (Lit a) (Lit b)) = (+) a b
eval (Add (Lit a) b) = (+) a (eval b)
eval (Add a (Lit b)) = (+) (eval a) b
eval (Add a b) = (+) (eval a) (eval b)

printExpr :: Expr -> String
printExpr (Add (Lit a) (Lit b)) = show a ++ " + " ++ show b
printExpr (Add (Lit a) b) = show a ++ " + " ++ (printExpr b)
printExpr (Add a (Lit b)) = printExpr a ++ " + " ++ show b
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)

notThe :: String -> Maybe String
notThe "the" = Nothing 
notThe a = Just a

replaceThe :: String -> String
replaceThe [] = ""
replaceThe a = unwords $ fromJust <$> replace <$> notThe <$> words a
    where replace x = if x == Nothing then Just "a" else x

countVowels :: String -> Integer
countVowels a = toInteger $ length $ filter (== True) $ (\x -> elem x vowels) <$> a


newtype Word' = 
    Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord x = if (ratio >= 0) then Just (Word' x) else Nothing
    where 
        ratio = con - vow
        con = len - vow
        vow = countVowels x
        len = (toInteger $ length x)


main :: ()
main = undefined