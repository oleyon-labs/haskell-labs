swap' a b f = f b a

type Swap a b = (b, a)

--полугруппа - множество с одной бинарной операцией

instance Semigroup Int where
    (<>) a b = (a + 2 * b)

class Semigroup a => Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a
    mappend' = (<>)
    mconcat' :: [a] -> a
    mconcat' = foldl mappend' mempty'

newtype Sum = Sum Int
    deriving(Show)
newtype Prod = Prod Int
    deriving(Show)

instance Semigroup Sum where
    (Sum x) <> (Sum y) = Sum $ x + y
instance Semigroup Prod where
    (Prod x) <> (Prod y) = Prod $ x * y

instance Monoid Prod where
    mempty = Prod 1

instance Monoid Sum where
    mempty = Sum 0