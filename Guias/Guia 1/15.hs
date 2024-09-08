{-
Ejercicio 15 ⋆
i. Definir el tip o RoseTree de árb oles no vacíos, con una cantidad indeterminada de hijos para cada no do.
i i. Escribir el esquema de recursión estructural para RoseTree. Imp ortante escribir primero su tip o.
i i i. Usando el esquema definido, escribir las siguientes funciones:
a ) hojas, que dado un RoseTree, devuelva una lista con sus ho jas ordenadas de izquierda a derecha,
según su aparición en el RoseTree.
b ) distancias, que dado un RoseTree, devuelva las distancias de su raíz a cada una de sus ho jas.
c ) altura, que devuelve la altura de un RoseTree (la cantidad de no dos de la rama más larga). Si el
RoseTree es una ho ja, se considera que su altura es 1.
-}

data RoseTree a = Rose a [RoseTree a]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT cRose (Rose v xs) =   cRose v (map (rec) xs)
    where rec = foldRT cRose

hojas :: RoseTree a -> [a]
hojas t = foldRT (\v xs -> v : concat xs) t 

distancias :: RoseTree a -> [Integer]
distancias = foldRT (\v xs -> if (null xs) then [0] else map (+1) (concat xs))

altura :: RoseTree a -> Integer
altura = foldRT (\v xs -> if null xs then 1 else 1 + maximum xs)

exampleTree1 :: RoseTree Int
exampleTree1 = Rose 1 [Rose 2 [Rose 4 [], Rose 5 []], Rose 3 []]
