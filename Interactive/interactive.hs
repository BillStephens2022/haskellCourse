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
