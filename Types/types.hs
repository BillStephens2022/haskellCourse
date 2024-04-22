
-- simple add function that takes a pair(tuple) of integers and returns an integer (i.e. the sum of those integers)
add :: (Int, Int) -> Int
add (x,y) = x+y

-- example of 'currying'.  Common design pattern for functions with multiple parameters.  
-- This does same as the add function above (i.e. adding two integers)
-- The function is applied to first parameter (x) and returns a new function and applies y and then returns the result
-- 'curried' functions take their arguments one at a time
add' :: Int -> (Int -> Int)
add' x y = x+y

zeroto :: Int -> [Int]
zeroto x = [0..x]

