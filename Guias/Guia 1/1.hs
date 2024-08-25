{-
Considerar las siguientes definiciones de funciones:
- max2 (x, y) | x >= y = x
| otherwise = y
- normaVectorial (x, y) = sqrt (x^2 + y^2)
- subtract = flip (-)
- predecesor = subtract 1


i. Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).

ii. Indicar cuáles de las funciones anteriores no están currifcadas. Para cada una de ellas, definir la función
currifcada correspondiente. Recordar dar el tipo de la función.

-}

--Currificada (toma 1 arg)
max2 :: (Float, Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

--Currificada (toma 1 arg)
normaVectorial :: (Float, Float) -> Float 
normaVectorial (x, y) = sqrt (x^2 + y^2)



-- flip :: (a->b->c) -> (b->a->c)
-- (-) :: (a->a->a) -> (a->a->a)
thisSubtract :: Float -> Float -> Float
thisSubtract = \x -> (\y -> flip (-) x y)

--Currificada (toma 1 arg)
predecesor :: Float -> Float
predecesor = subtract 1

--Currificada (toma 1 arg)
evaluarEnCero :: (Float -> b) -> b 
evaluarEnCero = \f -> f 0

dosVeces :: (a->a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [(a->b->c)] -> [(b->a->c)]
flipAll = map flip

flipRaro :: (a->b->c) -> a -> b -> c -- Ayuda
flipRaro = flip flip


