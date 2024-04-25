-- IO primitives

-- getChar :: IO Char
-- putChar :: Char -> IO ()  -- note: the IO means it returns nothing i.e. 'void', an empty tuple
-- return :: a -> IO a

-- getLine :: IO String
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO () -- adds a new line to the string

-- "do" notation allows us to string a number of actions together, one after the other

import System.IO

strlen :: IO ()
strlen = do 
    putStr "Enter a string: "
    xs <- getLine
    putStr "The string has "
    putStr (show (length xs))
    putStrLn " characters"

hello :: IO ()
hello = do
    putStr "Enter your name: "
    xs <- getLine
    putStr "Hello "
    putStr xs
    putStrLn "!"
