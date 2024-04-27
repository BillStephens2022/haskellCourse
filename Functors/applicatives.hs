-- Applicative Functors - functors that take functions with more than one argument
-- Generalizing the fmap function

-- fmap0 :: a -> f a
-- fmap1 :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d 
--   .
--   .  
--   .

-- > fmap (+) (Just 1) (Just 2)
-- Just 3
-- we could make a new class for functors that handle functions with 2 args and create another
-- one for 3 args, etc. i.e.
-- class Functor2 f where
    -- fmap2 ::  (a -> b -> c) -> f a -> f b -> f c
-- but ideally, we'd like a more general class that accepts any number of args

-- pure a -> f a  --embedding a value into a data structure
-- (<*>) :: f (a -> b) -> f a -> f b -- generalized form of function application

-- pure g <*> x <*> y <*> z  "applicative style" = ((g x) y) z

-- So, how do we define fmap0, fmap1, etc ?
-- fmap0 :: a -> f a
-- fmap0 = pure

-- fmap1 :: (a -> b) -> f a -> f b
-- fmap1 g x = pure g <*> x  (which = f (a -> b) f a which = f b)

-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 g x y = pure g <*> x <*> y

-- class Functor f => Applicative f where
    -- pure :: a -> f a
    -- (<*>) :: f (a -> b) -> f a -> f b 

-- Example: Maybe
-- instance Applicative Maybe where
    -- -- pure :: a -> Maybe a
    -- pure x = Just x
    -- -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    -- Nothing <*> mx = Nothing
    -- (Just g) <*> mx = fmap g mx

-- Examples
-- >pure (+1) <*> (Just 1) = Just 2
-- >pure (+)  <*> Just 1 <*> Just 2 = Just 3
-- >pure (+) <*> Nothing <*> Just 2 = Nothing

-- Lists
  -- instance Applicative [] where
    -- -- pure :: a -> [a]
    -- pure x = [x]
    -- -- (<*>) :: [a -> b] -> [a] -> [b]
    -- gs <*> xs = [g x | g <- gs, x <- xs]

-- List Examples
-- >pure (+1) <*> [1,2,3] = [2,3,4]
-- >pure (+) <*> [1] <*> [2] = [3]
-- >pure (*) <*> [1,2] <*> [3,4] = [3,4,6,8]  -applies multiplication against the two lists in all possible ways

