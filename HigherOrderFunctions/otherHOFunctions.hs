-- Other Library Functions

-- The library function (.) returns the composition of two functions as a single function
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = (\x -> f (g x))

-- Example: Let's define odd' as "not . even" using the composition operator "." where we
-- combine two functions "not" and "even"
-- this function will first check if number is even.  If then applies the "not" function to the
-- result of the "even" function.  So if the "even" function returns True, it will then apply the
-- "not" function to True, and return False as the final result.
odd' :: Int -> Bool
odd' = not . even
-- odd' 457 returns True
-- odd' 456 returns False

-- all' checks if all items in a list satisfy a predicate
-- takes in a predicate function (represented as: 'a -> Bool') and a list [a] as  
-- parameters.  Returns a boolean.
-- applies the predicate to each item (x) in the list (xs) and forms a list of the results, which
-- is a list of booleans.  It then applies 'and' to that list of booleans to return True or False.
-- using the 'and' function, all values in list must be True in order return True as the final result.
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]  
-- Example: all' even [2,4,68,712] returns True
-- all' even [2,5,68,712] returns False

-- take items from the start of the list until the predicate results in false

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- Example - take all of the items from the list until there is a blank space
-- takeWhile' (/= ' ') "abc def" returns "abc"

-- Example - take all of the items from this sorted list below 100:
-- takeWhile' (< 100) [1,2,55,88,99,100,101,789] returns [1,2,55,88,99]

-- does opposite of takeWhile'.  This will drop the leading items in the list that meet the predicate
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs

-- Example - dropWhile' (==' ') "     abc" returns "abc"  (this drops the leading spaces)
-- Example - drop all of the leading items from this sorted list below 100:
-- Example - dropWhile' (< 100) [1,2,55,88,99,100,101,789] returns [100, 101, 789]

-- Exercise:  Express [f x | x <- xs, p x] using map and filter
-- Answer:  map f (filter p xs)

