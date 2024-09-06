{-
Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
En caso contrario, explicar el motivo.
-}

-- Caso base devuelve valor fijo
-- ! Caso base accede a valores de xs con tail xs
-- No deberia ser estructural
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                        then [x]
                                        else x : elementosEnPosicionesPares (tail xs)

-- ? caso base devuelve funcion? 
-- ? Accede a la cola del segundo argumento, no seria estructural. No?
entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

-- ! Ayuda
