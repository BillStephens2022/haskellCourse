-- map - existing higher-order function which applies a function to every element of a list and returns the modified list
-- we'll redefine map below as map' so we can see type declaration and the definition and test it
map' :: (a -> b) -> [a] -> [b]  -- first argument is a function, 2nd argument is a list, returns a list
map' f xs = [f x | x <- xs]  -- "for each x in xs apply function (f) to x" - defined using a list comprehension
-- example: map' (+1) [1,3,5,7]  - this adds 1 to every item in the list and returns the udpated list [2,4,6,8] - basically passes an increment function to the map function

-- does same as map' but using a recursive definition
mapRecursive :: (a -> b) -> [a] -> [b]
mapRecursive f [] = []
mapRecursive f (x:xs) = f x : map f xs

-- the filter function - also in Haskell standard library, but we'll redefine here so we can see types/definition and to test examples
-- here the function is a predicate (returns a boolean) - so only elements meeting certain conditions (i.e. that satisfy the predicate) will be returned in the modified/filtered list
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x] -- apply a predicate function (p) to xs list.  for each element (x) in list (xs) apply the predicate function (p) to x, and if true, add to list
-- filter' even [1..10] returns [2,4,6,8,10] so it filters out only the even numbers from a list of 1 to 10

-- recursive definition for filter - does same as above

filterRecursive :: (a -> Bool) -> [a] -> [a]
filterRecursive p [] = []
filterRecursive p (x:xs)
    | p x = x : filterRecursive p xs
    | otherwise = filterRecursive p xs