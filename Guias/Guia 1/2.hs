uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

curry :: ( (a,b) -> c ) -> a -> b -> c
curry f x y = f (x,y)

--curryN tiene que devolver una version currificada de una cantidad arbitraria de argumentos
-- Lo que nos cambia es la evaluacion dependiendo de si arranca de izquierda o derecha,
-- Teniendo esto en cuenta podemos tomar una lista de argumentos y dependiendo de si hacemos un
-- foldr o un foldl va a darnos el valor de la funcion curryficada o uncurrificada
-- ? consultar
