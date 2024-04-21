double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

lastElement xs = xs !! (length xs - 1)
lastElement' xs = head (reverse xs)

removeLastElement xs = take (length xs - 1) xs
removeLastElement' xs = reverse (tail (reverse xs))
