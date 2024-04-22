
-- simple add function that takes a pair(tuple) of integers and returns an integer (i.e. the sum of those integers)
add :: (Int, Int) -> Int
add (x,y) = x+y

-- example of 'currying'.  Common design pattern for functions with multiple parameters.  
-- This does same as the add function above (i.e. adding two integers)
-- The function is applied to first parameter (x) and returns a new function and applies y and then returns the result
-- 'curried' functions take their arguments one at a time
add' :: Int -> Int -> Int
add' x y = x+y

zeroto :: Int -> [Int]
zeroto x = [0..x]

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

-- Polymorphic Functions
    -- A function is 'polymorphic' ('of many forms') if its type contains
    -- one or more type variables

    -- Example: length :: [a] -> Int 
    -- for any type 'a' length takes a list of values of type a and returns an integer
    -- type variables (i.e. 'a' above) must be or begin with a lower case letter

-- Overloaded Functions
    -- A polymorphic function is called "overloaded" if its type contains one or more
    -- class constraints.

    -- Example: (+) :: Num a => a -> a -> a
    -- Note: this is the type definition from the addition function from the 
    -- standard haskell library
    -- For any numeric type 'a', function '(+)' takes two values of type 'a' and
    -- returns a value of type 'a'.
    -- the 'Num a' before the '=>' is the class constraint.  It is constraining 'a' to
    -- be of type class 'Num'.
    -- Since this is constrained to 'Num', you can use types like Int and Float, but you
    -- can't use types like Char or String or Bool.

-- Type Classes
    -- Num - numeric types  (+) :: Num a => a -> a -> a
    -- Eq - equality types (i.e. allows for equality checks) (==) :: Eq a => a -> a -> Bool
    -- Ord - ordered types (i.e. can be sorted, put in order) (<) :: Ord a => a -> a -> Bool


twice f x = f (f x)

pair x y = (x, y)

double x = x * 2

palindrome xs = reverse xs == xs

--Exercises - 
-- 1) create same function using various methods (pattern matching, if/then/else, guards)
-- safetail - return empty list if list is empty otherwise return tail
-- safetail function using pattern matching
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- safetail using conditional expression
safetail' :: [a] -> [a]
safetail' xs = if null xs then [] else tail xs

-- safetail using guards
safetail'' :: [a] -> [a]
safetail'' xs
    | null xs = []
    | otherwise = tail xs
    
-- 2) write 3 possible definitions that could be used for logical OR operator:
        -- First:  False || False = False
        --         False || True = True
        --          True || False = True
        --          True || True = True

        -- Second: False || False = False
        --          _ || _ = True
        
        -- Third:   False || b = b
        --          True || _ = True
