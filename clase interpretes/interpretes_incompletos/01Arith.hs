
data Expr = EConstNum Int
          | EAdd Expr Expr

eval :: Expr -> Int
eval expr = case expr of
    EConstNum x -> x
    EAdd e1 e2 -> eval e1 + eval e2

