-- Source Language
data Expr = Val Int | Add Expr Expr

-- Semantics for the Source Language
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y


-- Virtual machine
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

-- Semantics for Target Language for our Virtual Machine
exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n:c) s = exec c (n:s)
exec (ADD:c) (m:n:s) = exec c ((n+m):s)

-- Compiler - translates from high level language to the target languange
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- Example
-- >  e = Add (Add (Val 2) (Val 3)) (Val 4)
-- >  eval e
-- this returns 9 (i.e. 2+3+4)
-- > exec (comp e) [] 
-- this returns [9]


-- Compiler correctness equation
-- exec (comp e) [] = [eval e]
--                  = (eval e):s



