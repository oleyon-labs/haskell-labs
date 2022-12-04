import Prelude hiding ((>>), (*>))
--fix - комбинатор неподвижной точки
--fix :: (a -> a) -> a
fix' f = let x = f x in x

--fix f = f (fix f)

data Stream' a = a :& Stream' a

constStream' :: a -> Stream' a
constStream' a = a :& constStream' a

--foldNat :: a -> (a -> a) -> Nat -> a
--foldNat z s Zero = z
--foldNat z s (Succ n) = s (foldNat z s n)

--x :: Nat -> a
--x Zero = z
--x (Succ n) = s (x n)

--x' :: Nat -> a
--x' = \nat -> case nat of
--    Zero -> z
--    Succ n -> s (x n)

--x'' = (\t -> \nat -> case nat of
--    Zero -> z
--    Succ n -> s (t n)) x

--x''' :: Nat -> a
--x''' = f x'''
--    where f = \t -> \nat -> case nat of
--                Zero -> z
--                Succ n -> s (t n)

--foldNat'' z s = fix f
--    where f t = \nat -> case nat of
--                    Zero -> z
--                    Succ n -> S (t n)

(.) ::(b -> b) -> (a -> b) -> (a -> c)
f.g = \x -> f (g x)

(>>.) :: (b -> b) -> (a -> b) -> (a -> c)
f >>. g = \x -> g (f x)

--Category
class Category cat where
    id :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

--f >> id == f
--id >> f == f
--(f >> g) >> h == f >> (g >> h)

--a -> m b
--a -> [b]

class Kleisli m where
    idk :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

--idk *> f == f
--f *. idk == f
--(f *> g) *> h == f *> (g *> h)
(+>) :: Kleisli m -> (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idk)
--обычн >> обычн == обычн
--спец +> обычн == спец
--спец *> спец == спец

