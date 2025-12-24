
data Expr = EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr

data Val = VN Int
         | VB Bool
  deriving Show

eval :: Expr -> Val
eval expr   = case expr of
  EConstNum n -> VN n
  EConstBool b -> VB b
  EAdd x y -> addVal (eval x) (eval y)

addVal :: Val -> Val -> Val
addVal (VN n1) (VN n2) = VN (n1 + n2)
addVal _ _ = error "Valores no son numericos"
