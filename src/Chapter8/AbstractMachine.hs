module AbstractMachine where


data Expr = Val Int | Add Expr Expr


type Cont = [Op]


data Op = EVAL Expr | ADD Int


-- A function that evaluates an expression in the context of a control stack
eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)


-- A function that executes a control stack in context of an integer
exec :: Cont -> Int -> Int
-- Return the integer of control stack is empty
exec []           n = n
-- If top of the stack is an EVAL operation, we evaluate y, placing the operation
-- ADD on top of the remaining stack.
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD y : c)  n = exec c (y + n)


-- A function that evaluates an expression to an integer by invoking `eval`
-- with the given expression and the empty control stack.
value :: Expr -> Int
value e = eval e []
