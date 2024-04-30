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

-- Let's check if this compiler correctness equation is actually true
-- Base case: e = Val n
-- exec (comp (Val n)) s
-- = exec [PUSH n] s
-- = n:s
-- = eval (Val n):s
-- Inductive case: e = Add x y
-- exec (comp (Add x y)) s
-- = exec (comp x ++ comp y ++ [ADD]) s
-- = exec (comp x ++ (comp y ++ [ADD])) s
-- now we can apply the distributivity lemma
-- = exec (comp y ++ [ADD]) (exec (comp x) s)
-- = exec (comp y ++ [ADD]) ((eval x):s)
-- = exec [ADD] (exec (comp y) ((eval x):s))
-- = exec [ADD] (eval y : eval x : s)
-- = (eval x + eval y) : s
-- = eval (Add x y) : s
-- Done!

-- Let's re write our comp function
quickComp :: Expr -> Code
quickComp e = comp' []

-- helper function comp'
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n:c
comp' (Add x y) c = comp' x (comp' y (ADD:c))

-- Compiler Correctness w/newly defined function
-- exec (comp' e c) s = exec c (eval e : s)
-- Base case: exec (comp' (Val n) c) s
-- = exec (PUSH n : c) s
-- = exec c (n : s)
-- = exec c (eval (Val n) : s)
-- Inductive case: exec (comp' (Add x y) c) s
-- = exec (comp' x (comp' y (ADD : c))) s
-- = exec (comp' y (ADD : c)) (eval x : s)
-- = exec (ADD : c) (eval y : eval x : s)
-- = exec c ((eval x + eval y) : s)
-- = exec c (eval (Add x y) : s)

-- Benefits from reformulating the compiler and compiler correctness theorem
-- 1) 22 steps initially in proof reduced to 8 steps
-- 2) original proof required 2 lemmas which was reduced to zero lemmas
-- 3) reduced use of append (i.e. '++')...more efficient
-- 4) original proof had a stack underflow issue, this reformulation does not






