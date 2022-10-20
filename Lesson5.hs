import System.Win32 (COORD(y))
import Text.Read (Lexeme(String))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(OneComponentRequestedSpec))
import Distribution.Compat.Semigroup (First')
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

name = Name fn ln

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

