-- Ejercicio 1 Considerar las funciones y 
-- Definir los tipos suponiendo que todo numero es tipo float
-- Indicar que funciones no estan curryficadas

-- Tener en cuenta que aca estan con los tipos definidos. Esto se lo agregue yo
-- El ejercicio solo viene con las funciones sin el tipado

max2 :: (Float, Float) -> Float
max2 (x,y) | x >= y = x
           | otherwise = y
-- No currificada.Version currificada
max2Curr :: Float -> Float -> Float
max2Curr x y | x >= y = x
             | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt (x^2+y^2)
-- No currificada. Version currificada
normaVectorialCurr :: Float -> Float -> Float
normaVectorialCurr x y = sqrt (x^2+y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)
-- Currificada

predecesor :: Float -> Float
predecesor = subtract 1
-- Currificada

evaluarEnZero :: (Float->a) -> a
evaluarEnZero = \f -> f 0
-- Currificada

dosVeces :: (a->a) -> (a->a)
dosVeces = \f -> f.f
-- Currificada 

flipAll :: [(a->b->c)] -> [(b->a->c)]
flipAll = map flip
-- Currificada


-- Ejemplo para entender este caso
flip1 :: (a1 -> b1 -> c1) -> b1 -> a1 -> c1

flip2:: (a2 -> b2 -> c2) -> b2 -> a2 -> c2
flip2 :: ((a1->b1->c1)->b1->a1->c1)->b2->a2->c2
flip2 :: ((a1->b1->c1)->b1->a1->c1)->b1->(a1->b1->c1)->a1->c1

-- Puedo sacar ((a1->b1->c1)->b1->a1->c1) ya que se aplica
-- Quedando b1->(a1->b1->c1)->a1->c1


flipRaro :: b -> (a->b->c) -> a -> c
flipRaro = flip flip
-- Currificada

-- //////////////////////////////////////////////

-- Ejercicio 2

-- 1. Definir funcion curry tal que dada una func de dos argumentos devuelve su equivalente currificada
curry :: ((a,b)->c)->a->b->c
curry f = \x -> (\y -> f (x,y)) 
 
curry2 :: ((a,b)->c)->a->b->c
curry2 f x y = f (x,y)
-- 2. Definir funcion uncurry que dada una func curryficada devuelve version no currificada
uncurry :: (a -> b -> c) -> (a,b)->c
uncurry f (x,y)= f x y

-- 3. Se podria definir una funcion curryN que tome una funcion de un numero arbitrario de argumentos y devuelva su version currificada?
-- TODO: Ninguna guia lo tiene resuelto. Chat gpt tira como pista (no pedi reso) falopa con definicion de tipos (type families)?
-- ! Segun chat, con tuplas normales no es posible porque haskell no tiene tuplas de aridad viable como un solo constructor parametrico. Le damos el beneficio de la duda y le creemos


