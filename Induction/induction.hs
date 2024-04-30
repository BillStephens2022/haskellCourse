--(++) :: [a] -> [a] -> [a]
--[] ++ ys = ys
--(x:xs) ++ ys = x : (xs ++ ys)

--reverse :: [a] -> [a]
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

-- how many evaluation steps does xs ++ ys take?
-- Example: [1,2] ++ [3]
-- = 1 : ([2] ++ [3])
-- = 1 : (2 : ([] ++ [3]))
-- = 1 : 2 : [3]

-- this took 3 steps to evaluate
-- in conclusion, xs ++ ys takes (length xs + 1) steps
-- this takes linear time to evaluate, which is not that efficient

-- How many steps for the reverse function?
-- Example:  reverse [1,2]
-- = reverse [2] ++ [1]
-- = (reverse [] ++ [2]) ++ [1]
-- = ([] ++ [2]) ++ [1]
-- this took 2 steps to evaluate the reverse function
-- now we need to add steps for evaluating the append ('++') function
-- from above we know that append takes length xs + 1 steps, which will be 3 steps
-- so, in general, reverse xs takes length 1 + 2 + ... +  (n + 1) where n = length xs 
-- using Gauss' formula to calculate a sum of a series of numbers from 1 to n = (n+1)(n+2) / 2 = (n^2 + 3n +2) /2
-- so this reverse function takes quadratic time to solve, which is very inefficient. basically because it uses '++' recursively

-- so, first let's derive a new function for reverse' which combines reversing xs and appending ys to it. So,
-- we know that the new function will have to have the same result as the one on the right below
-- reverse' xs ys = (reverse xs) ++ ys
-- base case: reverse' [] ys
-- = (reverse []) ++ ys
-- = [] ++ ys
-- = ys
-- inductive case: reverse' (x:xs) ys
-- = reverse (x:xs) ++ ys
-- = reverse (x:xs) ++ ys
-- = (reverse xs ++ [x]) ++ ys
-- = reverse xs ++ ([x] ++ ys)
-- = reverse xs ++ (x:ys)
-- = reverse' xs (x:ys)
-- therefore my recursive definition will be:  reverse' (x:xs) ys = reverse' xs (x:ys)

-- So now our final definition for reverse' will be:
reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)
-- note that in the final definition we eliminated "++", which we know is inefficient.

-- using this, we can now change our reverse function so that it doesn't include ++
-- this has the effect of turning something that was quadratic time to linear time complexity
quickReverse:: [a] -> [a]
quickReverse xs = reverse' xs []

-- Example quickReverse [1,2,3]
-- = reverse' [1,2,3] []
-- = reverse' [2,3] [1]
-- = reverse' [3] [2,1]
-- = reverse' [] [3,2,1]  -- now, we've reached the base case
-- = [3,2,1]

-- Fast Flatten (for Trees)
data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

-- because we are using the '++', this is inefficient
-- how can we eliminate the use of the '++'

-- similiar to the previous example let's come up with a helper function that combines the behavior of 
-- flattening and appending
-- flatten' t ns = flatten t ++ ns
-- base case: flatten' (Leaf n) ns
-- = flatten (Leaf n) ++ ns
-- = [n] ++ ns
-- = n:ns
-- so base case is now: flatten' (Leaf n) ns = n:ns
-- Inductive case: flatten' (Node l r) ns
-- = flatten (Node l r) ++ ns
-- = (flatten l ++ flatten r) ++ ns
-- = flatten l ++ (flatten r ++ ns)
-- = flatten l ++ (flatten' r ns)
-- = flatten' l (flatten' r ns)
-- we've now eliminated '++'

-- flatten' (Node l r) ns = flatten' l (flatten'r ns)

-- Completed function definition
flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n:ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

-- now we can use this to help us derive a more effecient function for flattening a tree

quickFlatten :: Tree -> [Int]
quickFlatten t = flatten' t []

-- now we have a much more efficient way of flattening a Tree structure







