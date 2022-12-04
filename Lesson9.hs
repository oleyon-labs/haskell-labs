--type Functor :: (* -> *) -> Constraint
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
  (<$<) :: a -> f b -> f a

instance Functor' [] where
    fmap' f [] = []
    fmap' f (x : xs) = f x : fmap' f xs

instance Functor' Maybe where
    fmap' f Nothing = Nothing
    fmap' f (Just a) =  Just (f a)