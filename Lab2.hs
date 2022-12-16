--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import Distribution.Simple.Utils (xargs, cabalVersion)
import Data.Char (isNumber, isDigit, isLetter)
--import Distribution.Simple.Setup (InstallFlags(installCabalFilePath))
--1. Придумать свой класс типов, который содержит как минимум две
--функции, одна из которых выражается через другие. Написать реализацию
--этого класса типов для любых двух типов данных, типы данных выбирать
--такие, чтобы их реализации отличались (можно использовать свои
--собственные типы данных).


class Sortable a where

    deconstruct :: a -> [Int]

    construct :: [Int] -> a

    sort :: a -> a
    sort xs = construct . qsort . deconstruct $xs
        where
            qsort :: Ord b => [b] -> [b]
            qsort [] = []
            qsort (x:xs) = qsort [l | l <- xs, l < x] ++ [x] ++ qsort [r | r <- xs, r >= x]


instance Sortable Int where
    deconstruct n = helper n []
        where
            helper 0 xs = xs
            helper n xs = helper (div n 10) (mod n 10 : xs)

    construct xs = helper 0 xs
        where
            helper n [] = n
            helper n (x : xs) = helper (n * 10 + x) xs

instance Sortable [Int] where
    deconstruct = id
    construct = id


--2. Создать тип данных бинарное дерево. И для него реализовать следующие вещи:
--2.1.Реализовать функцию преобразования дерева в список (toList).
--2.2.Реализовать функцию вычисления высоты дерева.
--2.3.Реализовать классы типов: Semigroup, Monoid, таким образом чтобы
--выполнялось условие: toList (mappend tree1 tree2) == mappend (toList
--tree1) (toList tree2).

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving(Show, Eq)


toList :: BinaryTree a -> [a]
toList tree = helper tree []
    where
        helper Empty xs = xs
        helper (Node a b c) xs = helper b xs ++ a : helper c xs

toBinaryTree :: [a] -> BinaryTree a
toBinaryTree [] = Empty
toBinaryTree xs = Node a (toBinaryTree (fst splitted)) (toBinaryTree (tail . snd $ splitted))
    where
        splitted = splitAt (div l 2) xs
        a = head . snd $ splitted
        l = length xs

height :: BinaryTree a -> Int
height tree = helper tree 0
    where
        helper Empty n = n
        helper (Node a l r) n = max (helper l n + 1) (helper r n + 1)



--mappend (toList . toBinaryTree $[1..10]) (toList . toBinaryTree  $[11..20])
--toList (mappend (toBinaryTree [1..10]) (toBinaryTree [11..20]))

tree1 = toBinaryTree [1,46,3,7]
tree2 = toBinaryTree [6,4,2,1]
tree3 = toBinaryTree [85,7,4,2,8,3,2]

trees = [tree1, tree2, tree3]

lists = map toList [tree1, tree2, tree3]
resultTree = mconcat trees
resultArray = mconcat lists

abob = resultArray == toList resultTree

lists1 = map toList trees1
trees1 = [tree1, tree11, tree3]

resultArray1 = mconcat trees1
resultTree1 = mconcat lists1

tree11 = Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))


abab = mappend (toList . toBinaryTree $[1..10]) (toList . toBinaryTree  $[11..20]) == toList (mappend (toBinaryTree [1..10]) (toBinaryTree [11..20]))
--abab1 = mappend (toList . toBinaryTree $[1..10]) (toList . toBinaryTree  $[11..20]) == toList (mappend (toBinaryTree [1..10]) (toBinaryTree [11..20]))

instance Semigroup (BinaryTree a) where
    (<>) a b = toBinaryTree . mappend (toList a) $ toList b

instance Monoid (BinaryTree a) where
    mempty = Empty

--3. Реализовать функцию, вычисляющую математическое выражение,
--записанное в виде строки. Строка может содержать операторы: +, -, *, /, ^
--(возводить можно только в целую положительную степень); функции: sin,
--cos (функция имеет наивысший приоритет); целые числа и вещественные
--с любым разделителем и скобки. Строка может содержать пробелы.
--Результатом может быть число, ошибка вычисления (например, деление
--на 0), ошибка парсинга (когда строка содержит некорректное выражение).
--Решение задачи можно разбить на три этапа:
--3.1.Разбиение строки на список токинов.
--3.2.Использование алгоритма Дейкстра для преобразования выражения из
--инфиксной формы в постфиксную форму.
--3.3.Вычисление результата.

data Token = Number Double | LBracket | RBracket | Operation Oper | Function String | ErrToken
    deriving(Show)

data Oper = Plus | Minus | Mult | Div | Pow
    deriving(Show)

data Tree a = Node a Tree Tree | Empty
    deriving(Show)




removeSpaces str = [c | c <- str, c `notElem` "\n \r\t"]



--getTokens str = formTokens str "" []
--    where
--        formTokens "" tokens = tokens
--        formTokens str tokens = formTokens (drop n str) token tokens



--getNextToken (c : str)
--    | isDigit c = getNumber str
--    |  = if (isDigit c) then getNumber str [c] else if (isLetter c) then getFunction str [c] else if (elem c "()+-/*^") then


--deleteUppercaseWords = unwords . filter (not . any isUpper) . words  --[if c `elem` ",.?!:;" then ' ' else c | c <- str]



getTokens [] = []
getTokens xs = helper (removeSpaces xs)
    where
        helper str = fst token : getTokens (drop (snd token) str)
            where token = getToken str

getToken [] = (ErrToken, 0)
getToken str@(c : xs)
    | c == '+' = (Operation Plus, 1)
    | c == '-' = (Operation Minus, 1)
    | c == '*' = (Operation Mult, 1)
    | c == '/' = (Operation Div, 1)
    | c == '^' = (Operation Pow, 1)
    | c == '(' = (LBracket, 1)
    | c == ')' = (RBracket, 1)
    | isDigit c = getNumber str
    | isLetter c = getFunction str
    | otherwise = (ErrToken, 0)

getNumber [] = (ErrToken, 0)
getNumber str = (Number (read helper :: Double), length helper)
    where
        helper = if (str !! length firstPart) == '.' then wholeNumber else firstPart
            where
                firstPart = takeWhile isDigit str
                secondPart = takeWhile isDigit $ drop (length firstPart + 1) str
                wholeNumber = firstPart ++ ('.' : secondPart)

getFunction [] = (ErrToken, 0)
getFunction str = (Function funName, length funName)
    where funName = takeWhile (\x -> isLetter x || isDigit x) str



--getOperationTree tokens 

-- "sin(34) + cos (12 +   32.123)/2^(3)"