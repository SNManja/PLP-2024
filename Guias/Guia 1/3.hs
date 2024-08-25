import qualified Control.Applicative as 2

-- 1.

foldrSum :: [a] -> a
foldrSum = foldr (+) 0

foldrElem :: [a] -> a -> Bool 
foldrElem l e = foldr (\x acc -> (x==e) || acc) False l

foldrPP :: [a] -> [a] -> [a]
foldrPP x y = foldr (:) y []
-- medio bobo, porque concatena 2 listas nomas

foldrFilter :: (a->Bool) -> [a] -> [a]
foldrFilter f l  = foldr (\x acc -> if (f x) then acc else x : acc) [] l  

foldrMap :: (a->b) -> [a] -> [b]
foldrMap f = foldr (\x acc -> (f x :acc) ) []


-- 2.

{-
Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
(>).
-}

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = (\x y -> if x >= y then x else y) [] 


-- 3.

{-
Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
-}

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldr (\x acc -> x+acc : acc) []

-- 4. 

{-
Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
-}


--5

{-
Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.
-}

sumaAltInv ::