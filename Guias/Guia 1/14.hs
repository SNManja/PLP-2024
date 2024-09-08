data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH :: (b -> a) -> (a -> a -> a) -> AIH b -> a
foldAIH cLeaf cBin t = 
    case t of  
        Hoja v -> cLeaf v
        Bin l r -> cBin (rec l) (rec r)  
    where rec = foldAIH cLeaf cBin

altura :: AIH a -> Integer
altura t = foldAIH (\ _ -> 1) (\a b -> if a >= b then 1 + a else 1 + b) t

arbolEjemplo :: AIH Integer 
arbolEjemplo = Bin (Hoja 5) (Bin (Hoja 10) (Hoja 15))