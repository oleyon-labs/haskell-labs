--{-# LANGUAGE FlexibleContexts #-}
import Data.Char(isUpper, isLower)
--import Data.List (permutations)
--import Data.ByteString (copy)
--import Data.ByteString (count, length)

--1
--Функция должна находить все вещественные решения уравнения 2го порядка,
--где a, b, c могут быть равны нулю. Функция должна принимать на вход параметры и возвращать решения.
--(Если решений нет, то использовать выражение error “Message” для отображения ошибки)
solveQuadraticEquation' :: (Ord a, Floating a) => a -> a -> a -> [a]
solveQuadraticEquation' 0 0 0 = error "infinite solution"
solveQuadraticEquation' 0 0 c = error "no solution"
solveQuadraticEquation' 0 b c = [- c / b]
solveQuadraticEquation' a b c
  | d > 0 = [(- b - sd) / (2 * a), (- b + sd) / (2 * a)]
  | d == 0 = [- b / (2 * a)]
  | otherwise = error "descriminant is less than zero"
  where
      d = b ^ 2 - 4 * a * c
      sd = sqrt d

--2
--Реализовать функцию sum’n’count :: Integer -> (Integer, Integer), 
--которая подсчитывает сумму цифр заданного числа и их количество
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count n = helper (abs n) 0 0
    where
        helper n s c
            | n == 0 = (s, c)
            | otherwise = helper (div n 10) (s + mod n 10) (c + 1)

--sum'n'count' :: a -> [a]
--sum'n'count' 0 = (0, 1)
--sum'n'count' n = takeWhile (\(x,y) -> x > 0 || y > 0) . iterate (\(x,y) -> x `divMod` 10) $ (n, 0)

--3
--Реализовать функцию, которая из заданной строки удаляет все слова,
--которые содержат хотя бы одну заглавную букву.
--Функция должна принимать в качестве параметра строку и возвращать строку.
--Функция должна состоять из композиции встроенных функций.
--(Для определения, что символ является заглавной/прописной буквой,
--используйте функции isUpper/isLower из модуля Data.Char, для
--подключения в начале файла добавьте строку: import Data.Char(isUpper, isLower))
deleteUppercaseWords :: String -> String
deleteUppercaseWords = unwords . filter (not . any isUpper) . words  --[if c `elem` ",.?!:;" then ' ' else c | c <- str]

--4
--Найти частичную сумму первых n членов ряда: ∑1/n^2.
--Функция должна принимать в качестве параметра количество слагаемых и возвращать сумму.
--Функция должна быть реализована с использованием функций zip, map или zipWith без явного использования рекурсии.
partialSum :: (Num c, Enum a) => (a -> c) -> a -> a -> c
partialSum f s n = sum . map f $[s..n]

--5
--Удалить из списка каждый третий элемент.
--Функция должна принимать в качестве параметра список и возвращать список.
dropEveryNElement :: Int -> [a] -> [a]
dropEveryNElement _ [] = []
dropEveryNElement n s = foldl (flip (:)) (dropEveryNElement n (drop n s)) (reverse . take (n - 1) $s)

--6
--Дан список из целых чисел, нужно выделить из него все неубывающие подпоследовательности максимальной длины.
--Функция должна принимать в качестве параметра список и возвращать список списков.
--Например, для списка [1,2,3,2,1,4,2,3,4] результат должен быть [[1,2,3],[2,3,4]] (порядок важен).
findBiggestNondecreasingSubsequences s = helper (findAllSubsequences (<=) s) [] 0
    where
        helper [] [] _ = []
        helper [] bs _ = bs
        helper (x:s) bs l
            | length x > l = helper s [x] (length x)
            | length x == l = helper s (x:bs) l
            | otherwise = helper s bs l

findFirstSubsequence f s = helper f s []
    where
        helper f [] [] = []
        helper f (x:s) [] = reverse . helper f s $[x]
        helper f s sub
            | null s = sub
            | otherwise = if f (head sub) (head s) then helper f (tail s) (head s : sub) else sub

findAllSubsequences f s = helper f s [] []
    where
        helper f [] [] [] = []
        helper f [] sub subs = reverse (reverse sub : subs)
        helper f (x:s) [] subs = helper f s [x] subs
        helper f s sub subs = if f (head sub) (head s) then helper f (tail s) (head s : sub) subs else helper f s [] (reverse sub : subs)

--7
--Дан список из уникальных элементов, нужно найти все перестановки заданного списка.
--Функция должна принимать в качестве параметра список и возвращать список списков.
--Например, для списка [1,2,3] результат должен быть [[1,2,3],[1,3,2],[[2,1,3],[2,3,1], [[3,2,1],[3,1,2]] (порядок не важен).
--permutations [] = []
--permutations s = helper s (length s - 1) [] []
--    where
        --helper [] _ pn ps = pn:ps
        --helper s 0 pn ps = ps
        --helper s c pn ps = helper (drop (c - 1) s ++ take (c - 2) s) (c - 1)  (s !! max 0 (c-2) : pn) ps
        --helper s c pn ps 
        --    | null s = pn:ps
        --    | c < 0 = ps
        --    | otherwise = helper (take (c + 1) s ++ drop c s) (c - 1)  (s !! c : pn) ps
--        helper s c pn
--            | null s = pn:ps
--            | c < 0 = 1
--            | otherwise = helper (take c s ++ drop (c + 1) s) (length s - 1)  (s !! c : pn) ps


permutations'q s = helper s []
    where
        helper [] _ = [[]]
        helper [x] ps = map (x:) (helper ps [])
        helper (x : s) ps = helper [x] (s ++ ps) ++ helper s (x : ps)

--permq = permutations

permutations'''' :: [a] -> [[a]]
permutations'''' xs = doPerm xs []
  where
    doPerm [] _ = [[]]
    doPerm [y] ys = map (y:) (doPerm ys [])
    doPerm (y : ys) zs = doPerm [y] (ys ++ zs) ++ doPerm ys (y : zs)

permutations'            :: [a] -> [[a]]
permutations' xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations' is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)





--rotations'' :: [a] -> [[a]]
--rotations'' xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

--perms'' :: [a] -> [[a]]
--perms'' []     = [[]]
--perms'' (x:xs) = concatMap (rotations''.(x:)) (perms'' xs)

--rotations'' :: [a] -> [[a]]
--rotations'' xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

--perms'' :: [a] -> [[a]]
--perms'' = foldr (\ x -> concatMap (rotations'' . (x :))) [[]]

permutations :: [a] -> [[a]]
permutations = foldr (\ x -> concatMap (rotations . (x :))) [[]]
    where rotations xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)