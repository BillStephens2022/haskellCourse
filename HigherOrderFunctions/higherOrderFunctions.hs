-- A function is called a 'higher-order' if it takes a function as 
-- an argument or returns a function as a result

-- Example - note the first argument below (i.e. '(a -> a)') is a function
-- this function is considered higher-order because it takes a function as an argument
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Let's pass in a lambda function
-- twice (\x -> x ^ 2) 9  returns 6561 (which is 9 squared, and then squared again... (9^2)^2 = 81^2 = 6561)

-- now let's create a custom function squared (which squares a number) and then pass that into the twice higher-order function
squared :: Int -> Int
squared x = x ^ 2

-- if we run "twice squared 9", it returns 6561, same as above where we used the lambda

