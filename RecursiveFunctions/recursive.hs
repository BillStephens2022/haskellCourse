-- simplest version
factorial :: Int -> Int
factorial n = product [1..n]

-- recursive version
fac :: Int -> Int
fac 0 = 1  -- this is the 'base case'
fac n = n * fac (n-1)

-- Example:   fac 3

-- fac 5 = 5 * fac (5 - 1) = 5 * fac 4 (1st (bottom) layer of calculation stack)
-- fac 4 = 4 * fac (4 - 1) = 4 * fac 3 (2nd layer of stack)
-- fac 3 = 3 * fac (3 - 1) = 3 * fac 2 (3rd layer of stack)
-- fac 2 = 2 * fac (2 - 1) = 2 * fac 1 (4th layer of stack)
-- fac 1 = 1 * fac (1 - 1) = 1 * fac 0 (5th layer of stack)
-- fac 0 = 1 (6th layer (top) of stack which is solvable since fac 0 is base case of function)
-- so if we put together above is


-- the above builds up in a stack, until we get to the base case 
-- we arrive at the base case when we get to the last line, where fac 0 is used.
-- we know from our function that fac 0 evaluates to 1.  Now we can start to solve.
-- fac 0 = 1
-- fac 1 = 1 * fac 0 (which we now know is 1) = 1
-- fac 2 = 2 * fac 1 (which we know based on above is 1) = 2
-- fac 3 = 3 * fac 2 (which we know based on above is 2) = 6
-- fac 4 = 4 * fac 3 (which we know based on above is 6) = 24
-- fac 5 = 5 * fac 4 (which we know based on above is 24) = 120
-- solved !
-- with recursion we basically build up a stack of calculations that depend on each other until
-- we get to the base case (at the top of the stack) which has a solution.  Then we pop off the
-- next calc in the stack resolve it, and so on
