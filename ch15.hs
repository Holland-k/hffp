import Data.Monoid
import Test.QuickCheck
import Control.Monad

data Optional a = 
    Nada | 
    Only a deriving (Eq, Show)

--newtype First' a
 --   = First' {getFirst :: Optional a} deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Data.Monoid.mempty 

instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    Nada <> (Only b) = Only b
    (Only b) <> Nada = Only b
    (Only b) <> (Only c) = Only (b <> c)

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String
madlibbin' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String

madlibbin' e adv noun adj = 
    e <> "! he said " <> 
    adv <> " as he jumped into his car " <> 
    noun <> " and drove off with his " <> 
    adj <> " wife."

madlibbinBetter' :: Exclamation
    -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat 
    [e, "! he said ", 
    adv, " as he jumped into his car ", 
    noun, " and drove off with his ", 
    adj, " wife."]

monoidAssoc :: (Eq m, Monoid m)
    => m -> m -> m -> Bool

monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    (First' Nada) <> (First' Nada) = First' Nada
    (First' Nada) <> (First' b) = First' b
    (First' b) <> _ = (First' b)

instance Monoid (First' a) where 
    mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = first'Gen

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
    a <- arbitrary
    frequency [ (1, return Nada), (10, return (Only a)) ]

first'Gen :: Arbitrary a => Gen (First' a)
first'Gen = do
    a <- optionalGen
    return (First' a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool

type FstId =
    First' String -> Bool

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a

test :: IO () 
test = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId) 
    quickCheck (monoidRightIdentity :: FstId)