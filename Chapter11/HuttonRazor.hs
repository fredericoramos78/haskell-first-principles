module Chapter11.HuttonRazor where

-- Hutton’s Razor8 is a simple expression language that expresses in- teger literals and the addition 
-- of values. The “trick” to it is that it’s recursive, and the two expressions you’re summing together 
-- could be literals or themselves further addition operations. This sort of datatype is stereotypical 
-- of expression languages used to motivate ideas in research papers and functional pearls. Evaluating 
-- or folding a datatype is also in some sense what you’re doing most of the time while programming anyway.

-- 1. Your first task is to write the “eval” function that reduces an expression to a final sum:

data Expr
       = Lit Integer
       | Add Expr Expr

eval :: Expr -> Integer 
eval (Lit i) = i 
eval (Add e1 e2) = eval e1 + eval e2


-- 2. Write a printer for the expressions:
printExpr :: Expr -> String 
printExpr (Lit i) = show i
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2


