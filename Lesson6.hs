

ex1 l1 [] = l1
ex1 l1 (x:xs) = ex1 (l1 ++ [x]) xs

ex2 l1 l2 = reverse . helper (reverse l1) $ l2
    where
        helper l [] = l
        helper l (x:xs) = helper (x : l) xs

l1 = [1..1000000]

l2 = [1..3]

lsum xs = foldl (+) 0 xs


type Name = String

data LastName' = LastName' String

newtype LastName = LastName String

showName (LastName n) = "Last name"

showName' (LastName' n) = "Last name'"

data List a = Empty | Cons a (List a)
    deriving(Show)


ourList = Cons 1 (Cons 2 (Cons 3 Empty))


elist = 1 : 2 : 3 : []


myHead Empty = error "List is empty"
myHead (Cons x xs) = x




myPrintList Empty = "[]"
myPrintList (Cons x xs)  = "[" ++ show x ++ helper xs
    where
        helper Empty = "]"
        helper (Cons x xs) = ", " ++ show x ++ helper xs

myPrintList' Empty = "[]"
myPrintList' (Cons x xs)  = concat ["[", show x, helper xs]
    where
        helper Empty = "]"
        helper (Cons x xs) = concat [", ", show x, helper xs]


reverse' lst = helper lst Empty
    where
        helper Empty list = list
        helper (Cons x xs) list = helper xs (Cons x list)

(+++) Empty l = l
(+++) (Cons x xs) l = Cons x (xs +++ l)


map' f Empty = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)

data Test = Test Int [Int]

data List2 = Empty' | Cons' Test List2

fun (Cons' (Test _ (x:xs)) ys) = x

data Nat = Zero | Succ Nat

toInteger' :: Nat -> Integer
toInteger' Zero = 0
toInteger' (Succ n) = 1 + toInteger' n

fromInteger' :: Integer -> Nat
fromInteger' 0 = Zero
fromInteger' n = Succ (fromInteger' (n - 1))

--instance Show a => Show (List a) where
--    show (Cons x xs) = Show x

instance Eq Nat where
    (==) a b = toInteger' a == toInteger' b

instance Ord Nat where
    compare a b = compare tia tib
      where
          tia = toInteger' a
          tib = toInteger' b