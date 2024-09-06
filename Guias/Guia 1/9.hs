{-
Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
to das de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]

i i. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la p osición i, j del resultado está el contenido de la p osición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, to das de longitud M , la salida deb e tener M listas, to das de longitud N .
trasponer :: [[Int]] -> [[Int]]
-}

sumaMat :: [[Int]] -> [[Int]] ->[[Int]]
sumaMat xm ym = zipWith (sumaFila) xm ym

sumaFila :: [Int]->[Int]->[Int]
sumaFila xs ys = zipWith (\x y -> x + y) xs ys

a::[[Int]]
a=[
    [1,2,3,4],
    [4,3,2,1],
    [1,2,3,4],
    [4,3,2,1]]


b::[[Int]]
b=[
    [3,2,1,0],
    [0,1,2,3],
    [3,2,1,0],
    [0,1,2,3]]
