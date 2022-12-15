import Prelude hiding (sequence, maybe, ($), (>>), (*>))
import System.Win32 (COORD(x))
--import Control.Arrow (Kleisli)
--import Control.Arrow (Kleisli)
--import Prelude hiding ((>>), maybe)
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x


class Category cat where
    id :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

instance Category (->) where
    id = \x -> x
    f >> g = \x -> g (f x)

class Kleisli m where
    idk :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idk)


instance Kleisli Maybe where
    idk = Just
    f *> g = f >> maybe Nothing g

next :: Char -> String
next 'a' = "ab"
next 'b' = "a"

instance Kleisli [] where
    idk = (: [])
    f *> g = f >> map g >> concat

generate :: Int -> (a -> [a]) -> (a -> [a])
generate 0 f = idk
generate n f = f *> generate (n - 1) f

($) :: (a -> b) -> a -> b
f $ a = (const a >> f) ()


(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()
(+$) :: Kleisli m => (a -> b) -> m a -> m b
f +$ a = (const a +> f) ()

infixr 0 $, +$, *$

--three = Succ (Succ (Succ Zero))
res = next *$ next *$ idk 'a'
res2 = next *$ tail $ next *$ reverse $ next *$ idk 'a'

-- ?? :: (a -> b -> c) -> m a -> m b -> m c
lift1 :: Kleisli m => (a -> b) -> m a -> m b
lift1 = (+$)

--lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
lift2 f a b = f' $$ b
    where f' = (+$) f a
--m (b -> c) -> m b -> m c
($$) :: Kleisli m => m (b -> c) -> m b -> m c
mf $$ ma = (+$ ma) *$ mf

res3 =Just (+2) $$ Just 2
res4 =Nothing $$ Just 2
res5 =[(+1), (+2), (+3)] $$ [10, 20, 30]

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idk [])

res6 = sequence [Just 1, Just 2, Just 3]
res7 = sequence [Just 1, Nothing]

res8 = sequence [[1,2,3], [11,22]]

mapK :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapK f = sequence . map f

-- +$ - функторы
-- +$ и $$ аппликативные
-- *$

class Functor f where
    fmap :: (a -> b) -> f a -> f b -- == (+$)

