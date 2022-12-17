import Data.Sequence (insertAt)
--type Functor :: (* -> *) -> Constraint
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
  --(<$<) :: a -> f b -> f a

--instance Functor' [] where
--    fmap' f [] = []
--    fmap' f (x : xs) = f x : fmap' f xs

instance Functor' Maybe where
    fmap' f Nothing = Nothing
    fmap' f (Just a) = Just (f a)

--fmap id = id
--fmap (f.g) = fmap f . fmap g

instance Functor' ((->) e) where
    fmap' = (.)
--  (a -> b) -> (e -> a) -> e -> b

fun1 = fmap' length tail

instance Functor' ((,) l) where
    fmap' f (a, b) = (a, f b)

--  fmap' :: (a -> b) -> (,) l a -> (,) l b

fmap'' f (a, b) = (a, f b)

instance Functor' (Either l) where
    fmap' _ (Left l) = Left l
    fmap' f (Right r) = Right (f r)


fun2 x = if x == 5 then Left "err" else Right 1

fun3 x = fmap' x

--f (b -> c) -> f b -> f c
res = fmap (++) ["123", "2", "1233"]

class Functor f => Applicative' f where
    pure' :: a -> f a
    (<*>>) :: f (a -> b) -> f a -> f b

res2 = (++) <$> ["123", "2", "1233"] <*> ["qwe", "asd"]

res3 = length <$> tail $ "qwerty"


--pure f <*> x = fmap f x
--pure id <*> u = u
--

instance Functor' [] where
    fmap' f [] = []
    fmap' f (x : xs) = f x : fmap' f xs


instance Applicative' [] where
    pure' a = repeat a--[a]
    --(<*>>) [] [] = []
    fs <*>> xs = zipWith (\f x -> f x) fs xs--[f x | f <- fs, x <- xs]



multiply :: Int -> Maybe Int
multiply x = Just (x * 2)

t = Just 4 >>= multiply >>= multiply

apply x = 
    pure x >>= (\x -> -- x = a;
    multiply x >>= (\y -> -- y = multiply x;
    multiply y >>= (\z -> -- z = multiply y;
    return z ))) -- return z;

-- m >>= return = m


--do { e1, e2} === e1 >> e2
--do {p <- e1, e2} === e1 >>= \p -> e2
--do {let v = e1, e2} === let v = e1 in do e2


apply2 a = do
    let x = a
    y <- multiply x
    z <- multiply y
    return (x, y, z)


main = do
    x <- getChar 
    putChar 'd'
    y <- getLine
    putStrLn y