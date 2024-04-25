import System.IO

hangman :: IO ()
hangman = do 
    putStrLn "Think of a word: "
    word <- sgetLine
    putStrLn "Try to guess it:"
    play word

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n' then
        do
            putChar x
            return []
    else
        do
            putChar '-'
            xs <- sgetLine
            return (x:xs)

-- reads a single Char from the keyboard without echoing it to the screen
getCh :: IO Char
getCh = do 
    hSetEcho stdin False  -- turns echoing off hSetEcho from System.IO library
    c <- getChar
    hSetEcho stdin True
    return c

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess ==  word then
        putStrLn "You got it!"
    else
        do
            putStrLn (match word guess)
            play word

-- function match indicates which chars in one string occur in a second string
match :: String -> String -> String
match xs ys = 
    [if elem x ys then x else '-' | x <- xs]
-- for example match "haskell" "pascal" returns "-as--ll"
