-- Ejercicios de clase esquemas de recursion

--(.) :: (b->c)->(a->b)->(a->c)
--(.) f g = \x-> f (g x)

flipAlt :: (a -> b -> c) -> (b->a->c)
flipAlt f y x = f x y 

-- ($) :: (a->b) -> a -> b
-- ($) f a = f a

-- const :: a -> b -> a
-- const x = \_ -> x

-- flip ($) 0 hace que se pase primero el valor. Estaria esperando una funcion para aplicarle el 0

-- Listas hay por
-- - Extension
-- - Secuencias
-- - Por comprension: ej: [(x,y)| x <- [0..5], y <- [0..3], x+y==4]

-- Hay tambien listas infinitas

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

listaMasCorta :: Ord a => [[a]] -> [a]
listaMasCorta [h] = h
listaMasCorta (x:xs) | (length x) > (length recursion) = recursion
                     | otherwise = x
    where recursion = (listaMasCorta xs)
{-
-- Generalizamos en mejor segun
mejorSegun :: (a->a->Bool)->[a]->a
mejorSegun f [x] = x
mejorSegun f (x:xs)  
    | f x m == True = x 
    | otherwise = m
    where m = (mejorSegun f xs) 
-}
filterFoldr :: (a->Bool)->[a]->[a]
filterFoldr f (x:xs) = foldr (filtrar) [] (x:xs)
    where filtrar a b | f a = a : b
                      | otherwise = b

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n = filter (\f -> f n == n)

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f = foldr aplicar []
    where aplicar a b = (f a): b 

-- Dada una lista de strings devuelve cada string en reversa
reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado = map reverse

-- Dada una lista de Int devuelve la lista con los num pares al cuadrado y los inpares sin modificar
paresCuadrados :: [Int] -> [Int]
paresCuadrados = map f 
    where f x | x `mod` 2 == 0 = x^2
              | otherwise = x

-- Pide definir una funcion que cumpla
-- listaComp f xs p = [f x | x <- xs, p x]
-- Pero con map y filter

-- En principio parece que toma los x en xs tal que (p x) se cumpla y guarda (f x)

listaComp :: (a -> b) -> [a] -> (a->Bool) -> [b]
listaComp f xs p = map f (filter p xs)  


-- ! Ejercicios de la guia

-- Ejercicio 3
-- Redefinir usando foldr las siguientes funciones sum, elem, (++), filter y map

sum :: Num a => [a] -> a
sum = foldr (\a b -> a + b) 0 

elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\a b -> a == e || b) False

mapAlt :: (a->b) -> [a] -> [b]
mapAlt f = foldr (\a b -> (f a) : b) []

--(++) :: [a] -> [a] -> [a]
--(++) xs ys = foldr (:) ys xs

-- Definir la funcion mejorSegun :: (a->a->Bool)->[a]->a que devuelve el maximo elemento de la lista segun una funcion comparacion utilizando foldr1.
mejorSegun :: (a->a->Bool)->[a]->a
mejorSegun f = foldr1 alpha
    where alpha a b | f a b = a
                    | otherwise = b

{-
Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
-}

sumasParciales :: Num a => [a] -> [a]
sumasParciales [] = []
sumasParciales (x:xs) = foldl (\acc x -> acc ++ [(last acc) + x]) [x] xs

{-
Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
-}

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> x - acc) 0 

{-
Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.
-}
sumaAlt2 :: Num a => [a] -> a
sumaAlt2 xs = foldr (\x acc -> x - acc) 0 (reverse xs)

-- Ejercicio 4
{-
Vamos a primero ver que funciones auxiliares recomienda.
take -> Toma los primer n elementos
drop -> Descarta los primeros n elementos
take junto a drop -> Permite separar un elemento, desarmar la lista
concatMap -> siendo f una funcion que genere listas, concateno el resultado de aplicar f por cada elemento
-}

{-
La idea aca es tomar un elemento x y separarlo
separar recursivamente.
Luego colocar dicho elemento en cada posicion por cada lista de listas

[1,2,3]

Toma 1. Sigue en [2,3]
Separa 2. Sigue en [3]
Separa 3. Vuelta
Posiciona 2 en cada posicion posible -> [2,3] , [3,2]
Posiciona 1 en cada posicion posible -> [1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]

Entonces esto como lo hacemos. 

map interior toma cada permutacion y aplica la lambda por cada elemento en [1..length p]
Esta lambda coloca en cada posicion posible el elemento

Luego el concatMap externo atravieza todas las permutaciones, aplica el map y luego concatena las listas de listas
-}
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = concatMap (\p -> map (\i -> (take i p)++[x]++(drop i p)) [0..(length p)]) (recursion)
    where recursion = permutaciones xs


{-
II. Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición.
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
(en algún orden).

Tomo la cabeza, hago partes de xs. Por cada caso tengo 2 casos uno donde tomo x y otro donde lo dejo
esto se aplica de forma recursiva
-}
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = concatMap (\p -> [p,x:p]) (partes xs)


{-
Quiero atravezar la lista de izquierda a derecha 
por paso quiero concatenar el acumulador como esta
y a su vez agregar a una copia del ultimo elemento el siguiente valor
-}
prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> acc ++ ([(last acc) ++ [x]])) [[]]


{-
Defnir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que
aparecen consecutivos en la lista original).
Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
(en algún orden).

Idea:
por indice quiero tomar el take i xs y el drop i xs
por ej [5,1,2]
[] [5,1,2]
[5] [1,2]
[5,1] [2]
[5,1,2] [2]
Pero vemos que los casos en el centro no van. 
Que tal si hacemos esto para cada xs

takeAndDrop (x:xs) = map (\i -> take i (x:xs)) [0..length (x:xs)] 
sublistas (x:xs) = takeAndDrop (x:xs) ++ sublistas (xs)

Aca tengo muchas listas repetidas. Dejo ya al ir haciendo en cada tail de la lista el proceso no vale la pena tomar el drop. Solo hago take

Esto escrito sin auxiliares:
-}
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = map (\i -> take i (x:xs)) [1..length (x:xs)] ++ sublistas (xs)


-- Ejercicio 5
{-
Primero veamos. Como se define una recursion estructural
“En una recursión estructural, la llamada recursiva se hace exclusivamente sobre un subdato inmediato definido por la estructura del tipo; en listas, ese subdato es la cola.” (gran frase de chat gpt)
-}

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = 
    if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)
-- Aca es explicito el llamado en el caso general a la elementosEnPosicionesPares de la tail de xs. Pero la recursion no es explicitamente sobre xs si no que la tail de xs. No es recursion estructural ya que no es subdato inmediato

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
    then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)
-- Se podria decir que aplica una recursion estructural entre (x:xs) y (y:ys) si ys estaria explicito. Pero al utilizar tail de ys para acceder a sus elementos esta deja de ser estructural. 

{-
Ejercicio 6 ⋆
El siguiente esquema captura la recursión primitiva sobre listas.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

a. Defnir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).
-}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a-> [a] -> [a]
sacarUna e = recr filtrado []
    where filtrado y ys acc | y==e = ys
                            | otherwise = y : acc

{-
b. 
Esto no se puede hacer con foldr porque debemos de poder devolver ys, 
de forma qeu no es posible con foldr que en la f solo podemos trabajar 
con el acumulador y el valor actual
-}

{-
c.
Definir la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento
-}

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e [] = [e]
insertarOrdenado e (x:xs) = recr insertador [] (x:xs)
    where insertador x xs acc | e <= x = (e : x : xs)
                              | null xs = x : [e]
                              | otherwise = x : acc

{-
i. Definir la función genLista :: a -> (a -> a) -> Integer -> [a], 
que genera una lista de una cantidad dada de elementos, 
a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. 
Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.
-}

genLista :: a-> (a->a) -> Integer -> [a]
genLista e f l = foldl (\acc x -> acc ++ [f (last acc)]) [e] [0..l]

{-
ii. Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.
-}

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (\v -> v + 1) (y-x+1)

-- Ejercicio 8
{-
Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
listas finitas e infinitas.

I. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.
-}

mapPares :: (a->b->c) -> [(a,b)] -> [c]
mapPares f (x:xs) = map (uncurry f) (x:xs)

{-
Puede aplicarse a listas infinitas? Si map es un derivado de foldr (recursion estructural). Esta se aplica de forma lazy. Puede ser aplicado en listas infinitas

II.  armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.
-}

armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x,y) : (armarPares xs ys)


{-

Esta solucion es una locura. Aca pasamos en el acumulador una funcion parcial, ahi esta la clave. Fue hecha chusmeando otras resos y repensandola en guardas
Que me parece mas estetico q el if en una linea

-}
armarParesCurry :: [a]->[b]->[(a,b)]
armarParesCurry = foldr armador (const []) 
    where armador x acc ys | null ys = [] 
                           | otherwise = (x,head ys) : (acc (tail ys))

{-
III. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
las dos listas. Esta función en Haskell se llama zipWith.
-}

mapDoble :: (a->b->c) -> [a] -> [b] -> [c]
mapDoble f (xs) (ys) = map (uncurry f) (armarParesCurry xs ys)

{-
Ejercicio 9

Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
-}

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+)) 


{-
II. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N.
trasponer :: [[Int]] -> [[Int]]
-}

trasponer :: [[Int]] -> [[Int]]
trasponer m = foldr (\i accMat -> (foldr (\j accCol -> (m !! j !! i) : accCol) [] [0..length m-1]) : accMat) [] [0.. (length (head m))-1] 
