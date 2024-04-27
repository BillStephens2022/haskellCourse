-- Monads

-- Example - A Simple Evaluator
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y
-- note this doesn't allow for possibility of failure (i.e. division by zero)
-- Example:  eval (Div (Val 12) (Val 4)) = 3
-- Failure Example: eval (Div (Val 12) (Val 0)) = ***Exception: divide by zero

-- to deal with possibility of failure we can use Maybe
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)
-- Example: safediv 16 0 = Nothing

-- mx >>= f = case mx of
    -- Nothing -> Nothing
    -- Just x -> f x
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- The first argument is a monadic value Maybe a.
-- The second argument is a function a -> Maybe b, which takes a 
-- normal value a and returns a monadic value Maybe b.
-- The result is a monadic value Maybe b.
-- This is also defined using a shorter notation of 
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Let's apply safediv to our eval function
-- issue is that safediv returns a Maybe, but Div is a pure mathematical function so
-- returns an Int.  So we need to use the '>>=' operator where we convert to Maybes
-- '>>=' takes a monadic value (a value wrapped in a monad) and a function that takes a normal value and 
-- returns a monadic value, then it "binds" the monadic value to the function.

safeEval :: Expr -> Maybe Int
safeEval (Val n) = Just n
safeEval (Div x y) = do n <- safeEval x
                        m <- safeEval y
                        safediv n m

-- safeEval (Div (Val 12) (Val 4)) = Just 3
-- safeEval (Div (Val 12) (Val 0)) = Nothing