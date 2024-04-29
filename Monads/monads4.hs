import Data.Char


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

-- convert a Char into an Int, where conversion can fail...(so we use Maybe Int)
conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- mapM' conv "12345" returns Just [1,2,3,4,5]
-- mapM' conv ['1','2'] returns Just [1,2]
-- mapM' conv "123a" returns Nothing

-- concat' will flatten out lists
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- this is a more generic version of concat'...it not only will flatten out lists, but all monadic types
join' :: Monad m => m (m a) -> m a
join' mmx = do mx <- mmx
               x <- mx
               return x

-- Examples
-- join' [[1,2],[2,3,4]] returns [1,2,2,3,4]
-- join' (Just (Just 1)) returns Just 1


-- Effectful Programming

-- Type             Effect
-- a -> mb          exceptions
-- a -> [b]         non-determinism
-- a -> ST b        internal state
-- a -> IO b        input/output

-- What is the point of Monads?
-- 1) Supports pure programming with effects
-- 2) Use of monads is explicit in types
-- 3) Can generalize functions to any effect

    data Expr a = Var a
                | Val Int
                | Add (Expr a) (Expr a)

    instance Monad Expr where
        -- return :: a -> Expr a
        return x = Var x
        -- (>>=) :: Expr a -> (a -> Expr b) -> (Expr b)
        (Var v) >>= f = f v
        (Val n) >>= f = Val n
        (Add x y) >>= f = Add (x >>= f)(y >>= f)



