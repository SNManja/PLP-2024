{-
i. Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una canti-
dad dada de elementos, a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.

i i. Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.
-}

genLista :: a -> (a->a) -> Integer -> [a]
genLista ini f len = reverse (foldr (\y acc -> if (length acc /=0) then (f (head acc)):acc else [ini]) [] [1..len])

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta a b = genLista a (\n -> n+1) (b-a + 1)

