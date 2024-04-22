
-- square a list of numbers, returns a list of squared numbers
-- extracting x from xs is called the generator ( "x <- xs" is the generator]
squareList :: Num a => [a] -> [a]
squareList xs = 
    [x^2 | x <- xs]

-- squareList [1..5] returns [1,4,9,16,25]

formListOfPairs :: [a] -> [b] -> [(a,b)]
formListOfPairs xs ys = 
    [(x,y) | x <- xs, y <- ys]

-- formListOfPairs [1,2,3] [4,5] returns [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- Dependent Generators
-- Later generators can depend on variables that are introduced by earlier generators
-- Example 1:  below, y is the 'later' generator and x is the earlier generator, so y can depend on x since
-- x was defined first
-- [(x,y) | x <- [1..3], y <- [x..3]]

-- Example 2
-- concat' function below (which is same as concat in standard Haskell library)
-- note that the x (later generator) is dependent on the xs (earlier generator)
-- function combines a list of lists into one list
concat' :: [[a]] -> [a]
concat' xss = 
    [x | xs <- xss, x <- xs]
-- concat' [[1,2,3],[4,5],[6,7,8],[9]] returns [1,2,3,4,5,6,7,8,9]

-- get all even numbers from a list and return them as a list
-- note that the filter that gets the even numbers (i.e. 'even x') is called a 'guard'.
getEvenNumbers :: [Int] -> [Int]
getEvenNumbers xs = 
    [x | x <- xs, even x]
-- getEvenNumbers [1..10] returns [2,4,6,8,10]

-- get all numbers evenly divisible by 7
getNumbersDivisibleBy7 :: [Int] -> [Int]
getNumbersDivisibleBy7 xs = 
    [x | x <- xs, x `mod` 7 == 0]
-- getNumbersDivisibleBy7 [1..100] returns [7,14,21,28,35,42,49,56,63,70,77,84,91,98]

-- the below get all factors of an integer and returns as a list
factors :: Int -> [Int]
factors n =
    [x | x <- [1..n], n `mod` x == 0]
-- factors 15 returns [1,3,5,15]

-- check if a number is prime, note that this uses the 'factors' function above
prime :: Int -> Bool 
prime n = factors n == [1,n]
-- prime 15 returns False
-- prime 7 returns True

-- get all prime numbers up to a specific integer
-- note this relies on 'prime' function above which relies on 'factors' function above that
getPrimeNumbers :: Int -> [Int]
getPrimeNumbers n = 
    [x | x <- [1..n], prime x]
-- getPrimeNumbers 100 returns [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-- function creates adjacent pairs using a list by zipping a list with its own tail
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
-- pairs [1,2,3,4] returns [(1,2),(2,3),(3,4)]
-- so it zipped [1,2,3,4] with its tail [2,3,4]

-- checks if a list is sorted using pairs function
-- all individual adjacent pair must be in order i.e. where we have a pair (x,y), x must be less than y
-- the 'and' below basically strings together the booleans that result from checking each pair - if all are true, it returns true.  if any false, returns false.
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]
-- sorted [1,2,3,4] = True
-- sorted [1,3,2,4] = False

-- searches for a value in a list and returns all of the Indices where that value shows up in the list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = 
    [i | (x',i) <- zip xs [0..], x == x']
-- positions 0 [1,0,0,1,0,1,1,0] returns [1,2,4,7]

-- counts how many times a character appears in a string
-- basically creates a list (using list comprehension) of each instance of the character being searched for and then gets its length
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
-- count 's' "Mississipi" returns 4

-- Exercises
-- 1) Pythagorean - create a funtion that gives a triple (x,y,z) of 
    -- positive integers where x^2 + y^2 = z^2

pyths :: Int -> [(Int, Int, Int)]
pyths n = 
    [(x,y,z) | x <- [1..n], y <- [x..n], z <- [x..n], x^2 + y^2 == z^2]

--pyths 5 returns [(3,4,5)]

-- 2) Perfect Numbers - check if a number is a perfect number
-- number is the sum of all its factors up to the given limit
-- i.e. 6 is a perfect number in that the sum of its factors up to (but not including 6) is 6.
-- factors of 6  = (1,2,3,6).  Not including 6, 1+2+3 = 6, so its a perfect number
perfect :: Int -> Bool
perfect n = sum (init (factors n)) == n

-- perfects will now give a list of perfect numbers up to the given number
perfects n = 
    [x | x <- [1..n], perfect x]

--3) Scalar product of two lists of products using a list comprehension
sp :: [Int] -> [Int] -> Int
sp xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
    where n = length xs
-- sp [1,2,4] [3,5,6] returns 37  (1*3) + (2*5) + (4*6)  = 37

-- same result as above using zip function
sp' :: [Int] -> [Int] -> Int
sp' xs ys = sum [x*y | (x,y) <- zip xs ys]
