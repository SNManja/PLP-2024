type Id = String 

data Expr = EConstNum Int
          | EConstBool Bool
          | EAdd Expr Expr
          | EVar Id 
          | ELet Id Expr Expr

data Env a = Env (Id -> a)
 -- ! Notar que aca desarrolle mi Env por colgado, se podia importar el modulo y ya
emptyEnv :: Env a  
emptyEnv = Env (\_ -> error "Variable unbound")

lookupEnv :: Id -> Env a -> a
lookupEnv id (Env env) = env id

extendEnv :: Id -> Env a -> a -> Env a
extendEnv newID (Env env) val = Env (\id -> if newID == id then val else env id)

data Val = VN Int
         | VB Bool
  deriving Show

addVal :: Val -> Val -> Val
addVal (VN a) (VN b) = VN (a + b)
addVal _ _           = error "Los valores no son numéricos."

eval :: Expr -> Env Val -> Val
eval expr           (Env env) = 
  case expr of 
    EConstNum n      -> VN n
    EConstBool b     -> VB b
    EAdd e1 e2        -> addVal (eval e1 (Env env)) (eval e2  (Env env))
    EVar id -> (lookupEnv id (Env env))
    ELet id e1 e2 -> eval e2 (extendEnv id (Env env) (eval e1 (Env env)))

ejemplo :: Expr
ejemplo =
  ELet "x"
    (EConstNum 5)
    (ELet "y" (EAdd (EVar "x") (EVar "x"))
      (ELet "y" (EAdd (EVar "x") (EVar "y"))
        (EVar "y")))

