import System.Win32 (COORD(y))
import Text.Read (Lexeme(String))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(OneComponentRequestedSpec))
import Distribution.Compat.Semigroup (First')
import GHC.Base (VecElem(Int16ElemRep))
str = "Hello" :: String
lchar = ['H','e','l','l','o']

type FunIntIntToInt = Int -> Int -> Int

funSum :: FunIntIntToInt
funSum = (+)

funAval :: FunIntIntToInt -> Int -> Int -> Int
funAval f x y = f x y

type FirstName = String
type LastName = String

showFullName' :: FirstName -> LastName -> String
showFullName' fn ln = fn ++ " " ++ ln

fn = "Ivan" :: String
ln = "Petrov" :: String

data Bit = Zero | One
    deriving (Show)

type Bits = [Bit]

zero = Zero

fromIntToBits :: Int -> Bits
fromIntToBits 0 = [Zero]
fromIntToBits 1 = [Zero]
fromIntToBits x = reverse . fromIntToBitsR $ x
    where
        fromIntToBitsR 0 = [Zero]
        fromIntToBitsR 1 = [Zero]
        fromIntToBitsR x =
            if even x
                then Zero : fromIntToBits (x `div` 2)
                else One : fromIntToBits (x `div` 2)

showBit Zero = "0"
showBit One = "1"

data Name = Name FirstName LastName
    deriving(Show)

--name = Name fn ln

showName :: Name -> String
showName (Name fn ln) = fn ++ " " ++ ln

type MiddleName = String

data FullName = FullName Name
    | FullNameWithMiddle Name MiddleName
    | ShortName Char Char LastName
    deriving (Show)

fullName = FullName . Name fn $ ln
fullNameWithMiddle = FullNameWithMiddle (Name fn ln) "Ivanovich"

shortName = ShortName 'I' 'I' ln

showFullName :: FullName -> String
showFullName (FullName (Name fn sn)) = fn ++ " " ++ sn
showFullName (FullNameWithMiddle (Name fn sn) mn) = fn ++ " " ++ mn ++ " " ++ sn
showFullName (ShortName n m ln) = concat [ln, " ", [n], ".", [m], "."]

data Person = Person {name :: String, age :: Int, sex :: Bool, height :: Int}
    deriving(Show)

person = Person {name = "Anna", age =  24, sex = True, height = 165}

getName :: Person -> String
getName (Person name _ _ _) = name

getAge :: Person -> Int
getAge (Person _ age _ _) = age

getSex :: Person -> Bool
getSex (Person _ _ sex _) = sex

getHeight :: Person -> Int
getHeight (Person _ _ _ height) = height

person2 = Person {name = "Anna"}

p = Person "Anna" --функция

p2 = Person (name person) (age person + 1) (sex person) (height person)

person3 = person {age = 25}

--l :: [Int -> Int -> Int]
l = [(5 +), (6 -), (7 *), div 8, mod 9]

l2 = map ($ 5) l

fun x y z = x - y + z

w = ((take 3 .) . (. map (^2))) (map (+ 1)) [1..10]

data Point a = Point a a
    deriving(Show)

pointInt = Point 5 5 :: Point Int
pointDouble = Point 5.1 5.5 :: Point Double
pointChar = Point 'r' 'z'

funSum' :: (Num a) => Point a -> a
funSum' (Point x y) = x + y