import System.Win32 (COORD(y, x), _open_osfhandle, nORM_IGNORECASE, BY_HANDLE_FILE_INFORMATION (bhfiCreationTime))
id' x = (\ x -> x) x

id'' x = (\ y -> x) 5

id2' = (\ x -> x)

div5 = ( / 5)

sum' x y = x + y

sum'' :: Int -> Int -> Int
sum'' x y = x + y

--div'' :: Num a => a -> a -> a
--div'' x y = x / y

div5' = (/) 5

test x y z = x + y * z

testX = test 5
testXY = test 2 3

testXZ = \ x -> test 2 x 5

factorial x = if x > 1 then x * factorial (x - 1) else 1

factorial' x
    | x == 1 = 1
    | otherwise = x * factorial' (x - 1)

fac' x
    | x == 1 = 1
    | otherwise = helper x 1
    where helper x acc
            | x == 1 = acc
            | otherwise = helper (x-1) (acc * x)

integralCalc f a b n = h * ((f a + f b) / 2 + sum)
    where
        h = (b - a) / n
        sum = helper (a + h) 0
        helper x acc
            | x >= b = acc
            | otherwise = helper (x + h) (acc + f x)
--integralCalc (\ x -> 3 * x ^ 2) 0 3 100

integralCalc' f a b n = h * ((f a + f b) / 2 + res a (n - 1) 0) --лучше работает
    where
        h = (b - a) / n
        res _ 0 sum = sum
        res a n sum = res (a + h) (n - 1) (f a + sum)

fib n = helper 1 1 n
    where
        helper n1 n2 n
            | n == 1 = n2
            | otherwise = helper n2 (n1 + n2) (n - 1)

fib' n
    | n == 1 = 0
    | n == 2 = 1
    | otherwise = fib' (n - 2) + fib' (n - 1)


fib'' 0 = 0
fib'' 1 = 1
fib'' 2 = 1
fib'' n = helper 0 1 n
    where
        helper pp p n
            | n == 0 = p
            | otherwise = helper p (pp + p) (n - 1)

fst' (x, _, _) = x
snd' (_, x, _) = x
trd' (_, _, x) = x


a = [1, 2, 3]
b = [1..10]
c = [0, 2..10]
d = [10, 9..1]
e = [(-10)..(-1)]
f = [0.1, 0.2..2.3] :: [Float]

i = [1..]

m = [x ^ y |
   x <- [1 .. 10], even x, y <- [1 .. 10], y < 4, y > 1]

maxxy = [max (x ^ 2) y |
    x <- [(- 5) .. (- 1)], x <= (- 2), y <- [1 .. 5], y >= 2]

-- x:xs
head' (x:_) = x
tail' (_:xs) = xs

length' (x)
    | null x = 0
    | otherwise = helper x 1
    where
        helper (x:xs) n
            | null xs = n
            | otherwise = helper xs (n + 1)

length'' [] = 0
length'' (x:xs) = 1 + length'' xs

length''' xs = foldr (\ x -> (+) 1) 0 xs

max'' (x:xs) = helper xs x
    where
        helper (x:xs) mx
            | null xs = mx
            | otherwise = if x > mx then helper xs x else helper xs mx


maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

take' n x = helper n x []
    where
        helper n x s
            | n <= 0 || null x = s
            | otherwise = helper (n-1) (tail x) (s ++ [head x])

take'' n x = helper n x []
    where
        helper n x s
            | n <= 0 || null x = reverse s
            | otherwise = helper (n-1) (tail x) (head x : s)
--дореализовать take

take''' :: Int -> [Int] -> [Int]
take''' 0 _ = []
take''' _ [] = []
take''' n (x:xs)
    | n < 0 = []
    | otherwise = x : take''' (pred n) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' el [] = False
elem' el (x:xs)
    | el == x = True
    | otherwise = elem' el xs

lf = [(^2), (+1),succ]
f' = (^2)

reverse'' :: [a] -> [a]
--reverse'' [] = []
reverse'' s = helper s []
    where
        helper xs xsn
            | null xs = xsn
            | otherwise = helper (tail xs) (head xs:xsn)


myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f ini [] = ini
myFoldl f ini (x:xs) = myFoldl f (f ini x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f ini [] = ini
myFoldr f ini (x:xs) = f x (myFoldr f ini xs)

reverse''' :: [a] -> [a]
reverse''' = foldl (\l x -> x : l) []

-- f (g (x))   f . g $ x

fun (a, b) = a + b