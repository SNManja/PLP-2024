partes :: [a] -> [[a]]
partes l = foldr (\x acc -> acc ++ insertarElemEnCU x acc) [[]] l

insertarElemEnCU :: a -> [[a]] -> [[a]]
insertarElemEnCU _ [] = []
insertarElemEnCU e (x:xs) = map (\(ys) -> e:ys) (x:xs)

-- Me jode el orden

prefijos :: [a] -> [[a]]
prefijos (x:xs) = filter (\(y:ys) -> y /= x) (partes (x:xs))