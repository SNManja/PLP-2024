data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)


evaluar :: Num a => a -> Polinomio a -> a
evaluar x pol = 
    case pol of
        X -> x
        Cte c -> c
        Suma a b -> evaluar x a + evaluar x b
        Prod a b -> evaluar x a * evaluar x b


-- ExPoli1 :: Polinomio Integer
-- ExPoli1 = Prod (Suma X (Cte (-1))) (Suma X (Cte 1))


