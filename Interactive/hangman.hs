import System.IO

hangman :: IO ()
hangman = do 
    putStrLn "Think of a word: "
    word <- sgetLine  -- defined below - binds the entered word to the word variable
    putStrLn "Try to guess it:"  -- prints this message to the screen
    play word  -- executes the play function with the word as an argument

sgetLine :: IO String
sgetLine = do
    x <- getCh -- defined below, get's the char typed into the console
    if x == '\n' then -- if it's a new line (i.e. user presses enter)
        do
            putChar x -- shows the char (which is just a carriage return) to the screen
            return [] -- returns an empty line
    else
        do
            putChar '-' -- shows a string of dashes - each dash representing a char in the entered word
            xs <- sgetLine -- binds the entered word to the variable xs
            return (x:xs)  -- returns the entered word as (x:xs)

-- reads a single Char from the keyboard without echoing it to the screen
getCh :: IO Char
getCh = do 
    hSetEcho stdin False  -- turns echoing off hSetEcho from System.IO library
    c <- getChar  -- gets the char typed into the console, binds to a variable
    hSetEcho stdin True -- sets back to True after it gets the char
    return c  -- returns the char typed into the console

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine  -- binds the entered guessed word to the guess variable
    if guess ==  word then
        putStrLn "You got it!"  -- print message if user successfully guesses word
    else
        do
            putStrLn (match word guess) -- prints the result of the match function which matches each letter of the actual word to the guessed word
            play word  -- recursive call to keep playing

-- function match indicates which chars in one string occur in a second string
-- xs is the guessed word
-- ys is the answer word
-- uses a list comprehension
-- generator (x <- xs) generates each character of the string
-- the if then else then takes each element of the string and checks if it is an element of
-- the answer.  if correct it shows the letter, if not, it shows a dash "-"
-- since a list comprehension it returns the list (in this case a list of chars aka a String) of 
-- letters/dashes so that user can make his/her next guess
match :: String -> String -> String
match xs ys = 
    [if elem x ys then x else '-' | x <- xs]
-- for example match "haskell" "pascal" returns "-as--ll"
