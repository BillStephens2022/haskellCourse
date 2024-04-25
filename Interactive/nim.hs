-- Game Board Representation
-- Each row has a descending number of stars
-- row 1: *****
-- row 2: ****
-- row 3: ***
-- row 4: **
-- row 5: *

-- 2 player game
-- each player will choose the number of stars to remove from the end of any row
-- can remove all stars in a row if you wish
-- aim of game is to be the player that leaves only one star on the board

import System.IO
import Data.Char -- contains a number of utility functions for working with Chars

-- Board Utilities
type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

-- all is a higher-order function that takes a predicate (a function that returns a boolean) and 
-- a list, and returns True if the predicate holds for all elements of the list, and 
-- False otherwise.
finished :: Board -> Bool
finished b = all (== 0) b

-- More on 'all', which is a higher order function that takes a predicate and a foldable type, which
-- in our case is a list and returns a boolean
-- all :: (a -> Bool) -> [a] -> Bool
-- all _ [] = True               -- Base case: an empty list satisfies all predicates
-- all p (x:xs) = p x && all' p xs -- Recursive case: check if the predicate holds for the current element and the rest of the list

-- check the board to check if row and num entered are valid - i.e. are there enough stars in the row selected to support what the user entered.
-- b = the current board, row = the row entered by user num = number of stars entered by user
-- if user entered a # of stars that exceeds the amount on the current board for that row, returns false
valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row - 1) >= num -- use -1 due to indexing beginning at zero

-- 'move' updates board based on user input
-- b = the current board, row = the row entered by user num = number of stars entered by user
-- in the list comprehension below, first we zip together the board with a list of 1-5.
-- This creates a list of tuples... so initially it is zipping the row number with the number of stars (r, n)
-- once this list of tuples is created via the zip, we can adjust it based on the user input (i.e. adjust r n)
-- if r is equal to the row entered, then take the current number of stars n and subtract 
-- the number of stars to be removed (as entered by user).  else n (current number of stars) remains the same.
-- returns the new adjusted board (which is a list)
move :: Board -> Int -> Int -> Board
move b row num = [adjust r n | (r,n) <- zip [1..5] b]
    where
        adjust r n = if r == row then n-num else n

-- I/O utilities
-- adds a new line on the screen (carriage return)
newline :: IO ()
newline = putChar '\n'

-- creates a string of stars based on n argument (# of stars)
-- stars 5 returns "*****"
stars :: Int -> String
stars n = concat (replicate n "* ")

-- prints a row of stars on the screen, where row = row number and num = number of stars
-- putRow 2 4 prints a row in this format: "2: ****"
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

-- this will print the board to the screen
-- if we use "putBoard intitial", it destructures the initial list into a, b, c, d, e which is
-- the number of stars for each row.  The do block below prints each row based on the row number
-- and the number of stars
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

-- prompt the user (i.e. for a row number or number of stars) and get result
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline -- defined above
                     if (isDigit x) then -- isDigit is defined in Data.Char library which is imported - checks if x (which is a Char) is a digit i.e. an number Char from '0' to '9'.
                        return (digitToInt x)  -- digitToInt (from Data.Char library) converts x from digit (which is a Char) to an Int
                     else
                        do newline -- defined above
                           putStr "you entered '"
                           putChar x
                           putStrLn "'"
                           putStrLn "ERROR: Invalid Digit!"
                           getDigit prompt -- recursively call function again until we get a valid digit

nim :: IO ()
nim = play initial 1

-- toggles between player 1 and player 2
next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player = 
    do newline
       putBoard board
       if finished board then
            do newline
               putStr "Player "
               putStr (show (next player))
               putStrLn " wins!!"
       else
            do newline
               putStr "Player "
               putStrLn (show player)
               r <- getDigit "Enter a row number: "
               n <- getDigit "Enter stars to remove: "
               if valid board r n then
                    play (move board r n) (next player)
               else
                    do newline
                       putStrLn "ERROR: Invalid Move!"
                       play board player

