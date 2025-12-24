
-- ! Ayuda, un centro para hacerlo con concatMap y pq el /= no va



{-
Definir la función permutaciones :: [a] -> [[a]],
que dada una lista devuelve todas sus permutaciones.

Se recomienda utilizar 
concatMap :: (a -> [b]) -> [a] -> [b], 
y también take y drop.

-}
{-
permutaciones :: [a] -> [[a]]
permutaciones l = foldr (\(xs) -> 
    \n -> concatMap 
    (\z -> map (\lista -> z!!n : lista) 
    ((permutaciones ((take n z) ++ (drop n+1 z)) 
    (length z-1)) 
    ++ 
    permutaciones (z) (n-1)))) [] l (length l -1)

-}

permutaciones :: [a] -> [[a]]
permutaciones l = foldr (\xs rec -> 
    \n-> concatMap (\z -> map (\lista -> z!!n : lista)) (permutaciones (take n z ++ drop n+1 z)) (n-1) ) l (n)


permutaciones :: [a] -> [[a]]
permutaciones l = foldr (\xs rec -> 
    \n -> permutaciones l (n-1) ++ concatMap () l
    )




