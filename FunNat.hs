module FunNat where

instance Num a => Num (t -> a) where
    (+) = fun2 (+)
    (*) = fun2 (*)
    (-) = fun2 (-)
    abs = fun1 abs
    signum = fun1 signum

fun1 = (.)
fun2 op a b = \t -> a t `op` b t