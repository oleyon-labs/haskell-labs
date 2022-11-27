--import qualified Prelude as P hiding (iterate, take, (!!))
import Prelude hiding (zipWith, zip, filter, map, iterate, take, (!!))
--import Data.Sequence (insertAt)
data Stream a = a :& Stream a

data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

nats :: Nat -> Stream Nat
nats a = a :& nats (Succ a)

constStream :: a -> Stream a
constStream a = a :& constStream a

head :: Stream a -> a
head (a :& xs) = a

tail :: Stream a -> Stream a
tail (a :& xs) = xs

(!!) :: Stream a -> Int -> a
(!!) (a :& xs) 0 = a
(!!) (a :& xs) n = (!!) xs (n - 1)

iterate :: (a -> a) -> a -> Stream a
iterate f n = n :& iterate f (f n)

take :: Int -> Stream a -> [a]
take 0 xs = []
take n (a :& xs) = a : take (n-1) xs

instance Show a => Show (Stream a) where
    show xs = showInfinity (show (take 5 xs))
        where showInfinity x = init x ++ "..."

map :: (a -> b) -> Stream a -> Stream b
map f (a :& xs) = f a :& map f xs

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a :& xs) = if f a then a :& filter f xs else filter f xs  

zip :: Stream a -> Stream b -> Stream (a, b)
zip (a1 :& xs1) (a2 :& xs2) = (a1, a2) :& zip xs1 xs2

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (a1 :& xs1) (a2 :& xs2) = f a1 a2 :& zipWith f xs1 xs2

