-- Basic Declarations
type Grid = Matrix Value
type Matrix a = [Row a]
type Row = [a]
type Value = Char

-- Easy Grid - note the dots represent blank cells in the grid
easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",                           
        "4..25...."]


-- Empty Grid
blank :: Grid
blank = replicate 9 (replicate 9 '.')

-- define rows
rows :: Matrix a -> [Row a]
rows = id
-- note that rows has the property: "rows . rows = id"

-- define columns
cols :: Matrix a -> [Row a]
cols =  transpose
-- Example: cols [[1,2],[3,4]] = [[1,3],[2,4]]. 
-- note that cols has the property: "cols . cols = id"

-- define boxes
boxs :: Matrix a -> [Row a]

-- note that boxs has the property: "boxs . boxs = id"

-- Validity checking
-- check if dupes are in grid by checking rows, columns, and boxes
valid :: Grid -> Bool
valid g =  all nodups (rows g) &&
           all nodups (cols g) &&
           all nodups (boxs g)

-- check for dupes in a list
nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs
