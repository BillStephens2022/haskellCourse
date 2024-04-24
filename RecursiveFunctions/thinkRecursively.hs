


-- 7 stage process for deriving a recursive function

-- Step 1 -- Name the function
-- Step 2 -- Write down the function's type
-- Step 3 -- Enumerate the cases for the function
-- Step 4 -- Define the simple cases (often the base case, but not always)
-- Step 5 -- List the "ingredients" - all of the items (functions, variables, constants, operators, etc)
-- Step 6 -- Define the other cases
-- Step 7 -- Think about the result.  Can the type be generalized more, or can we improve definition 
        -- (i.e. instead of Integers, generalize to Numbers.  
        -- Or instead of (x:xs) maybe you can use (_:xs) if x isn't used, etc)

--  Problem 1 - Define a function that sums a list of numbers
-- Step 1: function's name: 'sumList'
-- Step 2: function's type:  sumList :: [Int] -> Int
-- Step 3: enumeration of cases:  
    -- we basically have 2 cases to match on - one for an empty list, and one for a non-empty list
    -- sumList [], sumList (x:xs)
-- Step 4: define the simples cases
    -- the empty list is our simple case
    -- sumList [] = 0
-- Step 5: list the ingredients
    -- the function itself: sumList
    -- the variable x : first element of the list
    -- the variable xs : remainder of the list
    -- integer constants
    -- integer operators
-- Step 6: Define the other cases:
    -- sumList (x:xs) is the only other case
    -- sumList (x:xs) = x + sumList xs
-- Step 7: Think about the result
    -- instead of [Int] -> Int, type can be more general to accept more types of numbers
    --  Num a => [a] -> a
    -- can we simplify the definition ?
    -- we could use a fold instead of recursive definition:
        -- foldr (+) 0

--  2 ways we could define the function:
  -- recursive version:
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

    -- non-recursive version, using a fold
sumList' :: Num a => [a] -> a 
sumList' xs = foldr (+) 0 xs