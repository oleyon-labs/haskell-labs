import Control.Applicative

--main :: IO ()
--main = print "Happy New Year!"

--main :: IO ()
--main = putStr =<< f <$> getChar <*> getChar <*> getChar


f :: Char -> Char -> Char -> String
f a b c = reverse [a, b, c]


--putStr "URA" >> putChar ' ' >> putStr "CANICULI"
--fmap reverse $ getLine

--type FilePath = String
--readFile :: FilePath -> IO String

--writeFile :: FilePath -> String -> IO ()
--appendFile

main :: IO ()
main = msg1 >> getLine >>= read >>= append
    where 
        read file = readFile file >>= putStrLn >> return file
        append file = msg2 >> getLine >>= appendFile file
        msg1 = putStr "Input file: "
        msg2 = putStr "Input text: "