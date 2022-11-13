import Language.Haskell.TH (valD)
--main = putStrLn "hello world"
--func1 a b = a + b
--func2 a b = if a > b then a else b
discount :: (Ord a, Fractional a) => a -> a -> a -> a
discount price minPrice disc =
    if price < minPrice
        then price
        else price - (price - minPrice) * disc / 100

discEval price minPrice disc = price - (price - minPrice) * disc / 100

discount' price minPrice disc = let
    discVal = (price - minPrice) * disc / 100
    discEval' price minPrice disc = price - (price - minPrice) * disc / 100
    in if price < minPrice
        then price
        else price - discVal

discount'' price minPrice disc = 
    if price < minPrice
        then price
        else price - discVal
    where
        discVal = (price - minPrice) * disc / 100
        discEval' price minPrice disc = price - (price - minPrice) * disc / 100

--перевод 16 ричные цифры в 10 ричные
--hexNum hex =
--    if hex == 'a' then "10"
--    else if hex == 'b' then "11"
--    else "error"
translate ch =
    case ch of
        'A' -> "10"
        'B' -> "11"
        'C' -> "12"
        'D' -> "13"
        'E' -> "14"
        'F' -> "15"
        otherwise -> "error"

translate' 'A' = let {x = "10"; y = "11"} in x
translate' 'B' = "11"
translate' 'C' = "12"
translate' 'D' = "13"
translate' 'E' = "14"
translate' 'F' = "15"
translate' _ = "error"
    where {x = "10"; y = "11"}

translate'' val
    | val == 'A' = let {x = "100"; y = "110"} in x
    | val == 'B' = x
    | val == 'C' = y
    | val == 'D' = "13"
    | otherwise = "error"
    where {x = "10"; y = "11"}

conditionalFunc bool val1 val2 = if bool then val1 else val2

