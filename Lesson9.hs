import Data.Sequence (insertAt)
--type Functor :: (* -> *) -> Constraint
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
  --(<$<) :: a -> f b -> f a

instance Functor' [] where
    fmap' f [] = []
    fmap' f (x : xs) = f x : fmap' f xs

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