-- $! выполняет отложенные вычисления
fun ~(x:xs) = [1,2] -- ленивые сопоставления с образцом
fun [] = [] 

fun' [] = []
fun' l@(x:xs) = x : l

x = [1..10] !! 5

list = [(x1,x2) | x1 <- [1..100], x2 <- map (^2) [1..100]]

list' = zip l1 l2
    where
        l1 = [1..]
        l2 = map (^2) l1

funSum n = sum . map snd . take n $ list'



findFirstSubsequence f s = helper f s []
    where
        helper f [] [] = []
        helper f (x:s) [] = reverse . helper f s $[x]
        helper f s sub
            | null s = sub
            | otherwise = if f (head sub) (head s) then helper f (tail s) (head s : sub) else sub