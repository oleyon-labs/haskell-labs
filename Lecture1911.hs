--безымянные функции используются в композиционном стиле программирования
-- \x -> x
--профильтровать список чисел, выбрав числа больше 2 и меньше 10 и при этом четные
f1 = filter p
    where p x = x > 2 && x < 10 && even x

f2 = filter (\x -> x > 2 && x < 10 && even x)

filter' = \p -> \xs -> case xs of
    [] -> []
    (x:xs) -> let rest = filter p xs
                in if p x
                    then x:rest
                    else rest

squareGeron a b c = (\p->sqrt(p*(p-a)*(p-b)*(p-c))) ((a+b+c)/12)

--Data.Functions
--id x = x
--const a _ = a

onlyFive = const 5

(&&&) a = if a then id else const False

(...) f g = \x -> f(g x)

add a Zero = a
add a (Succ b) = Succ (add a b)

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)


--foldNat :: Nat -> (Nat -> Nat) -> Nat -> Nat
foldNat zero succ Zero = zero
foldNat zero succ (Succ b) = succ (foldNat zero succ b)

--add1 :: Nat -> Nat -> Nat
add1 = foldNat id (Succ .)

two = Succ (Succ Zero)
three = Succ two

