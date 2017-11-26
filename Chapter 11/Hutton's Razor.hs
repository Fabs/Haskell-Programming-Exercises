-- Hutton's Razor

data Expr = Lit Integer | Add Expr Expr

-- 1.

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = (+) (eval x) (eval y)

-- 2.

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)
