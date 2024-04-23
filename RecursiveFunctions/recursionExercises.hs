-- 1) Logical 'and'

and' :: [Bool] -> Bool
and' [] = True
and' (b:bs) = b && and' bs


-- 2) Concatenation

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- 3) Replicate

replicate' :: Int -> a -> [a]
replicate'  0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 4) Indexing Operator (!!)  (using !!! instead since !! already in standard library and !!' doesn't work)
(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

-- 5) Insertion Sort - Inserts an item into a sorted list into the correct position
insert' :: Int -> [Int] -> [Int]
insert' x [] = [x]
insert' x (y:ys) = 
    if x <= y then 
        x:y:ys else
            y:insert' x ys

-- 6) Insertion Sort - pass in an unsorted list and it will sort it by simply using the insert' function which we defined above
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert' x (insertionSort xs)

-- 7) merge - merge 2 sorted Lists - returned list should be sorted
mergeSortedLists :: [Int] -> [Int] -> [Int]
mergeSortedLists [] ys = ys
mergeSortedLists xs [] = xs
mergeSortedLists (x:xs) (y:ys) = 
    if x <= y then 
        x : mergeSortedLists xs (y:ys) else 
            y : mergeSortedLists (x:xs) ys

-- 8) merge sort - sort a list by dividing it into two and sorting & merging it using mergeSortedLists above

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = mergeSortedLists (msort ys) (msort zs)
    where ys = take n xs
          zs = drop n xs
          n = length xs `div` 2