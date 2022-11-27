class CloneAndAppend a where
    cloneAndAppend  :: a -> a

    cloneAndAppendList :: [a] -> [a]
    cloneAndAppendList = map cloneAndAppend

instance CloneAndAppend Int where
    cloneAndAppend = (* 2)

instance Num a => CloneAndAppend [a] where
    cloneAndAppend l = l ++ l
    cloneAndAppendList = id

class MyReversable a where
    myReverse :: a -> a

instance MyReversable [a] where
    myReverse = reverse

instance MyReversable Int where
    myReverse = read . reverse . show

class Num a => MyClasss a where
    func1 :: a -> a

-- * -> * -> *
data Ex1 a b = C1 a | C2 b

-- (* -> *) -> *
type Cont a = a Int


funEx :: Cont [] -> Cont []
funEx = id


