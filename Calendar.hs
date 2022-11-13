module Calendar where

import Prelude --(Int, Char, String, Show(..), (++))

-- Дата
data Date = Date Year Month Day

-- Год
data Year  = Year Int       -- Int это целые числа

-- Месяц
data Month  = January    | February   | March    | April          
            | May        | June       | July     | August   
            | September  | October    | November | December

data Day = Day Int

-- Неделя
data Week  = Monday     | Tuesday   | Wednesday 
           | Thursday   | Friday    | Saturday     
           | Sunday   

-- Время
data Time = Time Hour Minute Second

data Hour   = Hour   Int    -- Час
data Minute = Minute Int    -- Минута
data Second = Second Int    -- Секунда

instance Show Week where
    show Monday     = "Mon"
    show Tuesday    = "Tue"
    show Wednesday  = "Wed"
    show Thursday   = "Thu"  
    show Friday     = "Fri" 
    show Saturday   = "Sat"
    show Sunday     = "Sun"

instance Show Time where
    show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
    show (Hour h) = addZero (show h)

instance Show Minute where
    show (Minute m) = addZero (show m)

instance Show Second where
    show (Second s) = addZero (show s)


instance Show Month where
    show January = "01"
    show February = "02"
    show March = "03"
    show April = "04"
    show May = "05"
    show June = "06"
    show July = "07"
    show August = "08"
    show September = "09"
    show October = "10"
    show November = "11"
    show December = "12"

instance Show Year where
    show (Year y) = show y

instance Show Day where
    show (Day d) = addZero (show d)

instance Show Date where
    show (Date y m d) = show y ++ "." ++ show m ++ "." ++ show d

addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as     = as