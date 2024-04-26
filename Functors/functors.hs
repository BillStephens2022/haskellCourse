-- Abstarcting Programming Pattens

-- Note the patterns below for Incrementing a List and Squaring a List are very similar
-- base case is essentially the same
-- and the same pattern of recursion is used on the head and tail of the list

-- incrementing a list
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns  -- increment head and then increment each element in the tail recursively

-- squaring a list
sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns -- squre the head and then square each element in the tail recursively

-- if we abstract this common pattern out... we basically have the map function
-- here we'll use same definition as map in standard library but use map' 

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- now we can use this function to apply any function to a list and don't need to create
-- a new function for every possibility
-- so, instead of using the sqr or the inc functions, we can use the more generic map'
-- function. 
-- example:  map' (+1) [5,6,7,8]  results in [6,7,8,9] 
-- example:  map' (^2) [5,6,7,8]  results in [25,36,49,64]
-- example:  map' (^3) [2,4,6,8]  results in [8,64,216,512]

-- Generalizing Further
-- class Functor f where
    -- fmap :: Functor f => (a -> b) -> f a -> f b

-- The List Functor
  -- instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    -- fmap = map

-- The Maybe Functor
  -- data Maybe a = Nothing | Just a
  -- instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    -- fmap g Nothing = Nothing
    -- fmap g (Just x) = Just (g x)

-- For example:  
-- "fmap (+1) Nothing" results in "Nothing"
-- "fmap (*2) (Just 3)" results in "Just 6"
-- "fmap not (Just False)" results in "Just True"

-- The Tree Functor
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- g :: a -> b (function)
-- x :: a  (leaf of type a)
-- l, r :: Tree a (Tree of type a)

t :: Tree Int
t = Node (Leaf 1) (Leaf 2)

-- Example: "fmap (^3) t" results in "Node (Leaf 1) (Leaf 8)"
-- "fmap length (Leaf "abc")" results in "Leaf 3"
-- "fmap even t" results in "Node (Leaf False) (Leaf True)"

-- Why use functors??
-- 1) We can use same name, fmap, for functions that are essentially the same.
-- 2) We can define generic functions that work for any functorial type
        -- like what we did above for int and sqr above - we were able to use fmap instead and just pass in the function we want to use to apply to the functorial type (list, tree, etc)





