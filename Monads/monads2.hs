-- class Applicative m => Monad m where 
     -- (>>=) :: m a -> (a -> m b) -> m b
     -- return :: a -> m a
     -- return = pure
     -- to be a monad , you need the '>>=' operator and the 'return' operator

-- In Haskell, a Monad is a concept used to manage sequences of operations, 
-- particularly in cases where each operation might have some special rules 
-- or requirements. It's a way to deal with chaining actions together.

-- Imagine you have a series of tasks to do, like baking a cake. Each task depends 
-- on the outcome of the previous one, and some tasks might have special conditions. 
-- For instance, you can't put frosting on the cake until it's baked, and you can't 
-- bake it until you've mixed the batter. Here, Monad helps us manage these dependencies 
-- and conditions.

-- Let's see how it works in Haskell:

 -- Bind Function (>>=):
-- The core of Monad is the >>= function, pronounced as "bind". This function allows us 
-- to chain operations together. It takes a value wrapped in a Monad, does something with 
-- that value, and produces another Monad.

-- Return Function (return):
-- This function is used to put a value into a Monad. It's not like return in many other 
-- languages; it just wraps the value.

-- Do Notation:
-- Haskell provides a nice syntax called "do" notation, which simplifies working with Monads. 
-- 'Do' is shorthand for the repeated use of the ">>=" operator
-- It makes code more readable and looks like imperative programming.

-- Example:

-- Define a function that takes an integer and returns a Maybe integer
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- A function that adds two numbers safely using Maybe Monad
addSafely :: Int -> Int -> Maybe Int
addSafely x y = do
    a <- safeDivide 10 x  -- First, safely divide 10 by x
    b <- safeDivide 20 y  -- Then, safely divide 20 by y
    return (a + b)        -- Finally, return the sum

-- Here's what's happening:

-- safeDivide takes two integers and returns a Maybe Int, either a result or Nothing 
-- if there's a division by zero.  
-- addSafely takes two integers, divides 10 by the first one and 20 by the second one, 
-- then adds them together.
-- The do block allows us to sequence these operations as if they were regular imperative code.

-- If any of the division operations return Nothing, the whole computation results in Nothing. 
-- Otherwise, it gives us the sum.

-- So, in essence, Monads help us deal with computations that might fail or have some other 
-- special conditions, while keeping our code clean and readable. They're like a structured 
-- way to handle sequencing and errors in functional programming.

-- Example: Maybe (an instance of Monad)
-- instance Monad Maybe where
    -- -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    -- Nothing >>= f = Nothing
    -- Just x >>= f = f x

-- Other examples of Monad:

-- 1. Lists: Lists in Haskell represent sequences of values. The Monad instance for lists 
-- represents the idea of non-deterministic computations or computations with multiple 
-- possible outcomes. For example, the list [1, 2, 3] >>= (\x -> [x, x * 2]) would result 
-- in [1, 2, 2, 4, 3, 6].

-- 2. IO: The IO Monad is used to represent input and output actions. It allows sequencing of
-- input and output operations in a controlled manner. For example, 
-- getLine >>= (\line -> putStrLn ("You said: " ++ line)) reads a line from input and then 
-- prints it out.

-- 3. Either: The Either type represents computations that can either succeed with a result 
-- or fail with an error. It's often used for error handling. The Monad instance for Either 
-- allows us to chain computations while also handling errors. 
-- For example, Right 10 >>= (\x -> Right (x * 2)) would result in Right 20.

-- 4.  State: The State Monad represents computations that carry around a state which can be 
-- modified. It's often used for managing stateful computations in a functional way. The Monad 
-- instance for State allows sequencing of stateful computations. For example, 
-- get >>= (\x -> put (x * 2)) retrieves the current state, doubles it, and sets it as the new 
-- state.

-- 5. Reader: The Reader Monad represents computations that depend on a shared environment. 
-- It's often used for dependency injection. The Monad instance for Reader allows sequencing 
-- of computations that depend on some shared environment. For example, asks (\env -> env * 2) 
-- reads a value from the shared environment and doubles it.

--  Each instance of Monad provides a way to work with specific kinds of computations in a 
-- structured and composable manner.

-- Example: Lists
-- instance Monad [] where
    -- -- (>>=) :: [a] -> (a -> [b]) -> [b]
    -- xs >>= f = concat (map f xs)
    -- or alternatively we could use list comprehension xs >>= f = [y | x <- xs, y <- f x]
-- where '[a]' is 'xs' and 'a -> [b]' is 'f'.  now we are mapping the function f across the list xs and
-- returns a single list
pairs :: [a] -> [b] -> [(a,b)]  -- takes 2 lists as args, and returns a list of tuples with all possible pairs from the two lists
pairs xs ys = do x <- xs
              y <- ys
              return (x,y)

-- Example: pairs [1,2] [3,4] = [(1,3),(1,4),(2,3),(2,4)]

-- alternatively, instead of the monadic definition above, we could have used a list comprehension to 
-- define the pairs function:
   -- pairs xs ys = [(x,y) | x <- xs, y <- ys]

-- the benefit of using the "do" notation in the definition is that it works for any monad - not just lists - list
-- comprehensions will obviously only work on lists

-- Example: State
   -- type State = ...
   -- type ST a = State -> (a, State)  -- ST stands for 'state transformer'

   -- newtype ST a  = S (State -> (a, State))  ('newtype' is like a data declaration except that you can only use it with
     -- data types that have a single constructor.  I just makes sure that this constructor doesn't actually have any overhead
     -- at run time)
-- app :: ST a -> State -> (a, State)
-- app (S st) s = st s
-- instance Monad ST where
    -- -- return :: a -> ST a
    -- return x = S (\s -> (x,s))
    -- -- (>>=) :: ST a -> (a -> ST b) -> ST b
    -- st >>= f = S (\s -> )


-- More on newtype:  
-- In Haskell, newtype is a keyword used to define a new type, typically to provide type safety and abstraction 
-- without incurring the overhead of defining a completely new data structure. 
-- Here's what it's used for:

-- 1.  Creating Distinct Types: Sometimes you want to create a new type that's essentially the same as an existing one but 
-- with a different type. However, you want the compiler to treat them as distinct types to prevent mixing them up 
-- accidentally. newtype allows you to create these distinct types without any runtime overhead.

-- Example: 
-- newtype Meter = Meter Double
-- newtype Kilogram = Kilogram Double
-- Here, Meter and Kilogram are distinct types, even though they are both represented as Double. This helps to avoid 
-- accidentally using a Meter where a Kilogram is expected and vice versa.

-- 2.  Typeclass Instances: newtype allows you to define new instances of type classes for existing types without 
-- conflicting with existing instances.
-- Example: newtype MyInt = MyInt Int

-- instance Num MyInt where
    -- MyInt a + MyInt b = MyInt (a + b)
    -- MyInt a * MyInt b = MyInt (a * b)
    -- other Num methods...
-- Here, MyInt is a new type based on Int, but it has its own implementation of the Num type class. This allows you 
-- to define different behaviors for MyInt without affecting the behavior of regular Int.

-- 3. Performance Optimization: newtype has no runtime overhead since it only exists at compile time. This can be 
-- advantageous for performance-critical code where you want the benefits of abstraction without the performance 
-- cost of a full data structure.

-- 4. Enforcing Invariants: If you want to enforce certain invariants on your data, newtype allows you to do so by 
-- hiding the constructor.

-- Example:  newtype PositiveInt = PositiveInt Int
-- positive :: Int -> Maybe PositiveInt
-- positive n
--    | n > 0     = Just (PositiveInt n)
--    | otherwise = Nothing
-- Here, PositiveInt ensures that its value is always positive, thanks to the positive function.

--Overall, newtype is a lightweight and efficient way to introduce type safety, abstraction, and 
-- distinctness in Haskell code.


