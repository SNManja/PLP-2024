

{-

Definir las siguientes funciones para traba jar sobre listas, y dar su tip o. To das ellas deb en p o der aplicarse a
listas finitas e infinitas.

i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

i i. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada p osición, el elemento
corresp ondiente a esa p osición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.

i i i. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento corresp ondiente de
las dos listas. Esta función en Haskell se llama zipWith.
-}
{-
uncurry :: (a -> b -> c) -> ((a,b)->c)
uncurry f = \(a,b) -> f a b

curry :: ((a,b)->c) -> (a->b->c)
curry f = \a b -> f (a,b)
-}

mapPares :: (a -> b ->c) -> [(a,b)] ->[c]
mapPares f l = map (uncurry f) l

-- ! Como aprovecho la currificacion aca???
armarPares :: [a] -> [b] -> [(a,b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = [(x,y)] ++ armarPares xs ys


mapDoble :: (a->b->c) -> [a] -> [b] -> [c]
mapDoble f l1 l2 = mapPares f (armarPares l1 l2)