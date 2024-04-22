# Types Exercises

## 1. What are the types of the following values?
     Q: ['a','b','c']
     A: [Char]

     Q: ('a','b','c')
     A: (Char, Char, Char)

     Q: [(False, '0'),(True, '1')]
     A: [(Bool, Char)]

     Q: ([False, True], ['0','1'])
     A: ([Bool],[Char])

     Q: [tail, init, reverse]
     A: [[a] -> [a]]
     
## 2. Whare the type types of the following functions?
    Q: second xs = head (tail xs)
    A: second :: [a] -> a

    Q: swap (x, y) = (y, x)
    A: swap :: (a, b) -> (b, a)

    Q: pair x y = (x, y)
    A: pair :: a -> b -> (a,b)

    Q: double x = x * 2
    A: double :: Num a => a -> a
    -- Note: constrained to 'Num' type class since you can't multiply all types by 2, only numbers

    Q: palindrome xs = reverse xs == xs
    A: palindrome :: Eq a => [a] -> Bool
    -- Note: constrained to 'Eq' type class since you need to be able to compare/equate things (i.e. compare reverse xs to xs to see if they are equal)

    Q: twice f x = f (f x)
    A: twice ::  (a -> a) -> a -> a

