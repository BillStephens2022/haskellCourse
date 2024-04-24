-- Type declaration for Pos (position) - think of it as x,y coords
type Pos = (Double, Double)

-- using the Pos type, we can use it in a type declaration and define something:

-- let's define an origin as 0,0
origin :: Pos
origin = (0.0,0.0)

-- let's define a function that moves something 1 unit to the left
left :: Pos -> Pos
left (x,y) = (x-1.0, y)

-- Example:  'left origin' will return (-1.0,0.0)
-- Example:  'left (5.0,5.0)' will return (4.0,5.0)

myLocation :: Pos
myLocation = (7,8)

distance :: Pos -> Pos -> Double
distance a b = sqrt ((fst a - fst b)^2 + (snd a - snd b)^2)

-- Example "distance myLocation origin" will return 10.63014581273465


type Pair a = (a, a)

mult :: Pair Int -> Int
mult (m,n) = m*n
-- Example:  "mult (3,6)" returns 18
copy :: a -> Pair a
copy x = (x,x)
-- Example:  "copy 8" returns (8,8)


-- Type declarations can be nested - here custom type Pos is nested within the Trans type:
    -- type Pos = (Int, Int)
    -- type Trans = Pos -> Pos

-- However, they cannot be recursive
    -- i.e. "type Tree = (Int,[Tree])" would not be allowed

-- 'type' keyword is used when using type synonyms - i.e. when we are giving a name to a type that already exists
-- 'data' keyword is used when introducing a new custom type
-- type and constructor names must always begin with an uppercase letter

data Answer = Yes | No | Unknown deriving (Show)

answers :: [Answer]
answers = [Yes,No,Unknown]

flipAnswer :: Answer -> Answer
flipAnswer Yes = No
flipAnswer No = Yes
flipAnswer Unknown = Unknown

-- create new type called Shape which can be either a Circle or a Rect, which each take a parameter
-- Circle takes a parameter of Float which represents the radius
-- Rect takes 2 parameters which represents the length and width
-- Note: Circle and Rect can be viewed as functions that 'construct' values of type Shape
-- in ghci, if you get the type of Circle (i.e. :type Circle) you get "Circle :: Float -> Shape" - basically constructor functions
-- in ghci, if you get the type of Rect (i.e. :type Rect) you get "Rect :: Float -> Float -> Shape" - basically constructor functions
data Shape = Circle Float | Rect Float Float deriving (Show)

-- the square function takes a Float side length as a parameter (n) and returns a Rectangle with length of n and width of n
square :: Float -> Shape
square n = Rect n n

-- calculate the area of the Shape using pattern matching.
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x*y


-- data Maybe a = Nothing | Just a  (commented out since this is already part of Haskell standard library)
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)



