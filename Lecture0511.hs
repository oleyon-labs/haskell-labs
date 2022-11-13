module Lecture0511 where

--import Prelude

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

instance Num Nat where
    (+) a Zero = a
    (+) a (Succ b) = Succ (a + b)
    (*) a Zero = Zero
    (*) a (Succ b) = a + a * b
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n - 1))
    abs x = x
    signum Zero = Zero
    signum _ = Succ Zero
    negate _ = error "negate is undefined"

toInteger' :: Nat -> Integer
toInteger' Zero = 0
toInteger' (Succ n) = 1 + toInteger' n

meanInt::Int->Int->Double
meanInt a b = fromIntegral (a + b) / 2

--data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

data Tree = Empty | Node Int Tree Tree
    deriving (Show, Eq)

insert::Int->Tree->Tree
insert a Empty = Node a Empty Empty
insert a (Node n l r) = if a > n then Node n l (insert a r) else Node n (insert a l) r