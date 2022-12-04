module Kleisli where
import Prelude hiding ((*>), id, (>>), (+>))
import System.Win32 (COORD(x))
import Main (two)
class Category cat where
    id :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idk :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idk)

instance Category (->) where
    id = \x -> x
    f >> g = \x -> g (f x)

data Maybe a = Nothing | Just a
--a -> Maybe b
pred :: Nat -> Maybe Nat
pred Zero = Nothing
pred (Succ a) = Just a

instance Kleisli Maybe where
    idk = Just
    f *> g = \a -> case f a of
        Nothing -> Nothing
        Just b -> g b

--let pred2 = pred *> pred
--let pred3 = pred *> pred2
--let two = succ (succ Zero)
--pred two
--pred2 two
--pred3 two
--let beside = pred *> \a -> (a, a + 2)