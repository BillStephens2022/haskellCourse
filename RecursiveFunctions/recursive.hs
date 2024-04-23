-- simplest version
factorial :: Int -> Int
factorial n = product [1..n]

-- recursive version
fac :: Int -> Int
fac 0 = 1  -- this is the 'base case'
fac n = n * fac (n-1)

-- Example:   fac 3

-- fac 5 = 5 * fac (5 - 1) = 5 * fac 4 (1st (bottom) layer of calculation stack)
-- fac 4 = 4 * fac (4 - 1) = 4 * fac 3 (2nd layer of stack)
-- fac 3 = 3 * fac (3 - 1) = 3 * fac 2 (3rd layer of stack)
-- fac 2 = 2 * fac (2 - 1) = 2 * fac 1 (4th layer of stack)
-- fac 1 = 1 * fac (1 - 1) = 1 * fac 0 (5th layer of stack)
-- fac 0 = 1 (6th layer (top) of stack which is solvable since fac 0 is base case of function)
-- so if we put together above is


-- the above builds up in a stack, until we get to the base case 
-- we arrive at the base case when we get to the last line, where fac 0 is used.
-- we know from our function that fac 0 evaluates to 1.  Now we can start to solve.
-- fac 0 = 1
-- fac 1 = 1 * fac 0 (which we now know is 1) = 1
-- fac 2 = 2 * fac 1 (which we know based on above is 1) = 2
-- fac 3 = 3 * fac 2 (which we know based on above is 2) = 6
-- fac 4 = 4 * fac 3 (which we know based on above is 6) = 24
-- fac 5 = 5 * fac 4 (which we know based on above is 24) = 120
-- solved !
-- with recursion we basically build up a stack of calculations that depend on each other until
-- we get to the base case (at the top of the stack) which has a solution.  Then we pop off the
-- next calc in the stack resolve it, and so on

-- multiply all elements in a list to get a product
-- note: will do same as 'product' function from standard Haskell library
product' :: Num a => [a] -> a
product' [] = 1  -- base case
product' (n:ns) = n * product ns
-- product' [5, 2, 15, 3] = 450

-- walkthrough of product'
-- build the stack
-- product' [5, 2, 15, 3] 
--  = 5 * product' [2,15,3]
--  = 5 * (2 * product' [15,3])
--  = 5 * (2 * (15 * product' [3])) 
--  = 5 * (2 * (15 * (3 * product' [])))   --now we've hit base case because we know product [] = 1
--  = 5 * (2 * (15 * (3 * 1)))
--  = 450

-- write a recursive function to get the length of a list
-- note: will do same as 'length' function from standard Haskell library
listLength :: [a] -> Int
listLength [] = 0  -- base case
listLength (_:xs) = 1 + listLength xs

-- walkthrough of listLength
-- listLength [1,2,3]
--  = 1 + listLength [2,3]
--  = 1 + (1 + listLength [3])
--  = 1 + (1 + (1 + listLength []))
--  = 1 + (1 + (1 + 0))
--  = 3


-- write a recursive function to reverse the elements in a list
-- note: will do same as 'reverse' function from standard Haskell library
reverseList :: [a] -> [a]
reverseList [] = []  -- base case
reverseList (x:xs) = reverseList xs ++ [x]

-- walkthrough of reverseList
-- reverseList [1,2,3]
--  = reverseList [2,3] ++ [1]
--  = (reverseList [3] ++ [2]) ++ [1] 
--  = ((reverseList [] ++ [3]) ++ [2]) ++ [1]
--  = (([] ++ [3]) ++ [2]) ++ [1]
--  = [3,2,1]

-- write a recursive function for zipping lists
-- note: will do same as 'zip' function from standard Haskell library
zipLists :: [a] -> [b] -> [(a,b)]
zipLists [] _ = []  -- base case 1 = zip an empty list with anything, get back an empty list
zipLists _ [] = []  -- base case 2 = zip anything with an empty list, get back an empty list
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys
-- remember that the ':' is the cons (constructor) for a list. So, we are forming a list
-- with (x,y) being the head and "zipLists xs ys" being the tail.  This is how it knows that
-- we are ultimately forming a list which aligns with the return type of a list of tuples

-- walkthrough of zipLists

-- zipLists [1,2,3] ['a','b','c']
--  = (1,'a') : zipLists [2,3] ['b','c']
--  = (1,'a') : (2,'b') : zipLists [3] ['c']
--  = (1, 'a') : (2,'b') : (3, 'c') : zipLists [] []
--  = (1, 'a') : (2,'b') : (3, 'c') : []
--  = [(1, 'a') : (2,'b') : (3, 'c')]

-- write a recursive function for dropping a given number of items from a list
-- 2 parameters 
  -- Int - the number of items you want to drop from list
  -- the original list

-- write a recursive function to return a list with the given number of items removed 
-- note: will do same as 'drop' function from standard haskell library
dropFromList :: Int -> [a] -> [a]
dropFromList 0 xs = xs  -- base case, dropping zero from a list, return the same list
dropFromList _ [] = [] -- base case, dropping anything from an empty list, return an empty list
dropFromList n (_:xs) = dropFromList (n-1) xs

-- walkthrough of dropFromList
-- dropFromLists 2 [1,2,3,4,5]
--  = dropFromLists 1 [2,3,4,5]
--  = dropFromLists 0 [3,4,5]   -- we've hit a base case!
--  = [3,4,5]

-- write a recursive function to append 2 lists (will do same as '++' operator)
addTwoLists :: [a] -> [a] -> [a]
addTwoLists [] ys = ys
addTwoLists (x:xs) ys = x : addTwoLists xs ys

-- walkthrough of addTwoLists
-- addTwoLists [1,2,3] [4,5]
--  = 1 : addTwoLists [2,3] [4,5]
--  = 1 : 2 : addTwoLists [3] [4,5]
--  = 1 : 2 : 3 : addTwoLists [] [4,5] - we've reached a base case!
--  = 1 : 2 : 3 : [4,5]
--  = [1,2,3,4,5] 



