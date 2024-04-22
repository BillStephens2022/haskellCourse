-- odds function not using a lambda, but using a 'where' clause and 
-- defining the one time use function
-- maps the function to an array of numbers.  
-- Will apply the function to each item in the array from 1 to n-1
odds :: (Num b, Enum b) => b -> [b]
odds n = map f [0..n-1]
    where
        f x = x*2 + 1

-- odds function using a lambda
-- mapping a lambda function to an array of numbers
odds' :: (Num b, Enum b) => b -> [b]
odds' n = map (\x -> x*2 + 1) [0..n-1]
