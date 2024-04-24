-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b


-- an implementation of foldr using list type, which is more specific than the actual type
-- definition for foldr above which is looking for something (t) of type Foldable.  A List is
-- a Foldable type.
-- type 'b' is the accumulator, so when we pass in the function, one of the parameters is the
-- accumulator.  The 2nd (paramter after the function passed in) is also of type 'b' which is
-- the initial value of the accumulator (which for summing is often initialized to zero, or for
-- products is 1)
-- note the recursive definition
foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList f v [] = v
foldrList f v (x:xs) = f x (foldrList f v xs)


-- Example: foldrList (+) 0 [1,2,3] returns the sum of each item in the list, which is 6.
-- adds each item to the initial value of the accumulator which was set to zero.
-- breaking it down:
--  = foldrList (+) 0 (1:(2:(3:[])))  -- [] is base case so it returns zero since that is initial value of v (the accumulator)
--  = 1 + (2 + (3 + 0))
--  = 6

-- Example:  foldrList (*) 1 [4,5,6] returns the product of all items in the list, which is 120.
-- accumulator is set to 1.
-- breaking it down:
--  = foldrList (*) 1 (4:(5:(6:[]))) -- [] is base case so it returns 1 since that is initial value of v (the accumulator)
--  = 4 * (5 * (6 * 1))
--  = 120

-- Example:  foldrList (\x acc -> x * x + acc) 0 [1,2,3] returns the sum of the squares of each
-- item in the list, which is 14.  The lambda function passed in, is squaring each item (x) of
-- the list and then adding it to the accumulator which was initialized to zero

-- Although foldr encapsulates a simple pattern of recursion, it can be used to define
-- many more functions than might first be expected.
-- For example, the length function is similar 
-- it is basically folding up an accumulated count of each element:
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length xs
-- Example:  length' [201..210] is 10

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- Example: reverse' [201..210] is: [210,209,208,207,206,205,204,203,202,201]