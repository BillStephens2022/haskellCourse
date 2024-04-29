-- The State Monad --

-- instance Monad ST where
    -- -- return :: a -> ST a
    -- return x  = S (\s -> (x,s))
    -- -- (>>=) :: ST a -> (a -> ST b) -> ST b

-- st >>= f = S (\s -> Let (x, s') = app st s
                    -- = in app (f x) s')

-- Example - Re-labelling Trees - we'll relabel the characters in a tree from chars to Ints (i.e. from 'a', 'b', 'c' to 1, 2, 3)
data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Char
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

--Tree a -> Int -> (Tree Int, Int)

-- The hard way
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf x) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
                        where (l', n') = rlabel l n
                                       = rlabel r n'

-- The easier way using state monads
-- First, we set up a state transformer helper function to give fresh integer labels, incrementing by 1 each time,
-- so that the updated state (i.e. the updated/incremented n can be used for the next leaf of the tree)
fresh :: ST Int
fresh = S (\n -> (n, n+1))

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x) = do n <- fresh  -- grab a 'fresh' label using helper function above and bind to 'n'
                     return (Leaf n)  -- return a Leaf with the n (integer) label

mlabel (Node l r) = do l' <- mlabel l  -- recursively label the l's and r's of each node
                       r' <- mlabel r
                       return (Node l' r')

-- Top level function to relabel a tree using the mlabel function above
label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 0)




