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

-- 'Maybe' Types - in cases where functions can fail, you may want to return a 'Maybe' type instead of letting program crash
-- For example in the safediv function below, if we divide by zero, we return 'Nothing' instead of allowing the program to crash.  Returns a 'Just' value if it succeeds.
-- similar to safediv, safehead will return a Nothing in case an empty list is passed as an argument to the function.
-- data Maybe a = Nothing | Just a  (commented out since this is already part of Haskell standard library)
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Recursive Data Types
-- In Haskell, new types can be declared in terms of themselves
-- Nat is a new type, with constructors Zero :: Nat and Succ :: Nat -> Nat
-- Zero, which has no paramters, and Successor which takes Nat as a parameter
-- 2 constructors: Zero which is a Nat, and Succ which is a function which takes an existing Nat as a parameter and returns a new Nat
-- this is basically giving us a procedure for producing an infinite sequence of values
-- For example, the value Succ (Succ (Succ Zero)) represents the natural number 1 + (1+ (1 + 0)) = 3
data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n
-- Example: "nat2int (Succ (Succ (Succ Zero)))" returns 3

int2nat :: Int -> Nat 
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
-- Example:  "int2nat 7" returns "Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))"

addNat :: Nat -> Nat -> Nat
addNat m n  = int2nat (nat2int m + nat2int n)
-- Example: "addNat (Succ (Succ Zero)) (Succ (Succ (Succ (Succ Zero))))", which adds 2 and 4, returns Succ (Succ (Succ (Succ (Succ (Succ Zero))))), which is 6

-- does same as above but defined recursively
addNat' :: Nat -> Nat -> Nat
addNat' Zero n = n
addNat' (Succ m) n = Succ (addNat' m n)

-- recursive multiplication function
multNat :: Nat -> Nat -> Nat
multNat Zero m = Zero
multNat (Succ n) m = addNat' (multNat n m) m  -- note: uses repeated addition (using addNat' defined above) to simulate multiplication

-- Arithmetic Expressions
-- Consider a simple form of expressions built up from integers using addition and multiplication
-- Using recursion, a suitable new type to represent such expressions can be declared by:
data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr

-- Expr has 3 constructors, one for Val, one for Add, and one for Mul
-- Val takes one parameter of type Int
-- Add & Mul each take 2 parameters of type Expr

-- for example, the Tree, 1 + 2 * 3 would be represented as follows:
-- Add (Val 1) (Mul (Val 2) (Val 3))
-- now we can create 2 functions - 1) size: to give the size (based on number of number inputs), and 2) eval: to evaluate/calculate the expression

-- Using recursion, it is now easy to define functions that process expressions.  For example:
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y
-- Example: "size (Add (Val 1) (Mul (Val 2) (Val 3)))" returns 3

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
-- Example: "eval (Add (Val 1) (Mul (Val 2) (Val 3)))" returns 7

-- Binary Tree data type - using a recursive data type
data Tree a = Leaf a
    | Node (Tree a) (Tree a)


