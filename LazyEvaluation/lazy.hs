
-- GENERATING PRIMES
-- Generate the infinite sequence of primes
-- 1. write down the infinite sequence 2, 3, 4, ...;
-- 2. mark the first numbers as being prime;
-- 3. delete all multiples of p from the sequence;
-- 4. return the second step

-- This is called the "Sieve of Aristopohenes"

-- this takes advantage of lazy evaluation of infinite lists and produces an infinite list of primes
primes :: [Int]
primes = sieve [2..]  -- 'sieve' is a helper function we define below

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

-- example:
-- >take 10 primes
-- result:  [2,3,5,7,11,13,17,19,23,29]

-- >takeWhile (<20) primes
-- result:  [2,3,5,7,11,13,17,19]

-- > primes !! 104  (element at index 104 of the primes list)
-- result:  571

-- >29 `elem` primes
-- result: True

-- >4 `elem` primes - infinite loop because 4 isn't a prime number so it keeps searching the
  -- infinite loop forever ...

twin (x,y) = y == x+2
-- > twin (3,5)
-- result: True
-- > twin (1,2)
-- result: False
twins = filter twin (zip primes (tail primes))
-- creates a seemingly infinite list (we actually don't know if it is infinite) of all twin primes
-- >take 5 twins
-- result: [(3,5),(5,7),(11,13),(17,19),(29,31)]
