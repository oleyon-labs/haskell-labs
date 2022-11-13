--data Nat = Zero | Succ Nat
--data T = Name T1 T2 ... TN - произведение типов значение нового типа T состоить из значений предыдущих?
--data Time = Time Hour Minute Second
{-# LANGUAGE GADTs #-}

data Boolll = True11 | False11


data Booll where
    True1 :: Booll
    False1 :: Booll

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

--константы в хаскеле представляют собой деревья - листья содержат примитивные конструкторы, а узлы - составные
--с помощью композиции строим из простых деревьев сложные
--с помощью декомпозиции разбиваем составные деревья на простейшие
--эти операции участвуют в операции объявления синонима
--show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s
--в хаскеле можно строить функции, передавая в ф-ию меньшее число аргументов (частичное применение)


map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr f z xs)

