limpiar :: String -> String -> String
limpiar _ [] = []
limpiar b a = filter (\x -> not (elem x b)) a

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\n -> n - promedio) xs
  where
    promedio = (sum xs) / fromIntegral (length xs)

todosIguales :: [Int] -> Bool 
todosIguales [] = True
todosIguales (x:xs) = all (\n -> x == n) xs
