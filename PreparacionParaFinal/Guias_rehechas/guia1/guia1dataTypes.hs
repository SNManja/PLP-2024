

{-
Ejercicio 10 ⋆
i. Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).
ii. Utilizando foldNat, definir la función potencia.
-}
-- Aca no aclara si los naturales tienen o no al 0. Tomamos como que no.
-- I.
foldNat :: b -> (Integer -> b -> b) -> Integer -> b
foldNat b f 1 = b
foldNat b f n = f n (foldNat b f (n-1))


-- II.
potencia :: Integer -> Integer -> Integer
potencia n pow = foldNat n (\x acc -> n * acc) pow



{-
Ejercicio 11
Definir el esquema de recursión estructural para el siguiente tipo:
data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)
Luego usar el esquema definido para escribir la función evaluar :: Num a => a -> Polinomio a -> a
que, dado un número y un polinomio, devuelve el resultado de evaluar el polinomio dado en e
-}

data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)

evaluar :: Num a => a -> Polinomio a -> a
evaluar _ (Cte a) = a
evaluar v X = v
evaluar v (Suma p1 p2) = (evaluar v p1) + (evaluar v p2)
evaluar v (Prod p1 p2) = (evaluar v p1) * (evaluar v p2)

-- Pero el ejercicio me pide hacerlo definiendo el esquema de rec estructural

foldPoly :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoly x fa fSuma fProd (Cte a) = fa a
foldPoly x fa fSuma fProd X = x
foldPoly x fa fSuma fProd (Suma p1 p2) = fSuma (foldPoly x fa fSuma fProd p1) (foldPoly x fa fSuma fProd p2)
foldPoly x fa fSuma fProd (Prod p1 p2) = fProd (foldPoly x fa fSuma fProd p1) (foldPoly x fa fSuma fProd p2)


evaluarFold :: Num a => a -> Polinomio a -> a
evaluarFold n pol = foldPoly n id (+) (*) pol
   



{-
Ejercicio 12
Considerar el siguiente tipo, que representa a los árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)
i. Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos.
-}
data AB a = Nil | Bin (AB a) a (AB a)

arbol :: AB Int 
arbol = (Bin (Bin (Nil) 1 (Bin Nil 2 Nil)) 3 ((Bin Nil 2 Nil)))

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB nullVal f Nil = nullVal
foldAB nullVal f (Bin r1 val r2) = f (foldAB nullVal f r1) val (foldAB nullVal f r2)

recAB :: b -> (b-> a -> b -> AB a -> AB a -> b) -> AB a -> b 
recAB nullVal _ Nil = nullVal
recAB nullVal f (Bin r1 val r2) = f (recAB nullVal (f) r1) val (recAB nullVal (f) r2) r1 r2 

{-
ii. Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB).
-}

esNil :: AB a -> Bool
esNil = foldAB True (\rec1 v rec2 -> False && rec1 && rec2) -- Bastante ridiculo hacerlo asi pero fue

altura :: AB a -> Integer
altura = foldAB 0 (\r1 v r2 -> (max r1 r2)+1) 

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\r1 v r2 -> 1 + r1 + r2)

{-
iii. Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil.
-}
mejorSegunAB :: (a->a->Bool) -> AB a -> a
mejorSegunAB f (Bin baser1 baseV baser2) = foldAB baseV (\r1 v r2 -> selector (f) (r1) (selector (f) r2 v)) (Bin baser1 baseV baser2)
    where selector f v1 v2 | (f v1 v2) == True = v1
                           | otherwise = v2
                           
{-
iv. Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho.
-}


arbolBin :: AB Int 
arbolBin  = Bin (Bin Nil 1 Nil) 2 (Bin Nil 3 Nil)


esABB :: Ord a => AB a -> Bool
esABB = recAB True (\r1 v r2 a1 a2 -> (if esNil a1 then True else (mayorIgual v (maximoAB a1))) && (if esNil a2 then True else (menor v (minimoAB a2))) && r1 && r2) 
    where 
        mayorIgual v1 v2 = (v1 >= v2)
        menor v1 v2 = (v1 < v2)


maximoAB :: Ord a => AB a -> a
maximoAB a = mejorSegunAB (\v1 v2 -> v1 >= v2) a

minimoAB :: Ord a => AB a -> a
minimoAB a = mejorSegunAB (\v1 v2 -> v1 < v2) a


{-
V. Justificar la elección de los esquemas de recursión utilizados para los tres puntos anteriores.
-}

{-
Utilizamos fold cuando queremos hacer recursion estructural, rec para recursion primitiva
Esto quiere decir que con fold hacemos cuando basta solo con ver el resultado de la operacion en ambas ramas y el valor del nodo actual
y en caso de rec la utilizamos cuando necesitamos poder acceder a las subramas del arbol, para hacer alguna operacion como la comparacion q hacemos en este caso
-}

{-
Ejercicio 13.
Dado el tipo AB a del ejercicio 12:
i. Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.
-}

ramas :: AB a -> [[a]]
ramas Nil = []
ramas a = recAB [] (\rec1 v rec2 a1 a2 ->  
    if (esNil a1 && esNil a2) 
        then [[v]]
    else 
        foldr (\l rec -> ((v:l) : rec)) [] (rec1 ++ rec2)) a
    
          
{-
ii. Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos
tienen la misma forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y
recordar el ejercicio 8.
-}



mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura a b = foldAB esNil comp a b
    where comp rec1 v rec2 Nil = False
          comp rec1a va rec2a (Bin rec1b vb rec2b) = True && (rec1a rec1b) && (rec2a rec2b)

{-
Se desea modelar en Haskell los árboles con información en las hojas (y sólo en ellas). Para esto introduciremos
el siguiente tipo:

data AIH a = Hoja a | Bin (AIH a) (AIH a)

a) Definir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita.
b) Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.
-}

data AIH a = Hoja a | ABin (AIH a) (AIH a)

-- a (Use ABin porque Bin ya esta utilizado por ej previo)
foldAIH :: (a->b) -> (b->b->b) -> AIH a -> b
foldAIH fHoja f (Hoja a) = fHoja a
foldAIH fHoja f (ABin r1 r2) = f (foldAIH fHoja f r1) (foldAIH fHoja f r2)

-- b 
alturaAIH :: AIH a -> Integer 
alturaAIH = foldAIH (const 1) (\r1 r2 -> 1+(max r1 r2)) 


tamanoAIH :: AIH a -> Integer 
tamanoAIH = foldAIH (const 1) (\r1 r2 -> 1+r1+r2) 

aih1_example :: AIH Int
aih1_example = ABin (ABin (Hoja 1) (ABin (Hoja 2) (Hoja 1))) (Hoja 2)

{-
Ejercicio 15 ⋆
i. Definir el tipo RoseTree de árboles no vacíos, con una cantidad indeterminada de hijos para cada nodo.

ii. Escribir el esquema de recursión estructural para RoseTree. Importante escribir primero su tipo.

iii. Usando el esquema definido, escribir las siguientes funciones:
a) hojas, que dado un RoseTree, devuelva una lista con sus hojas ordenadas de izquierda a derecha,
según su aparición en el RoseTree.
b) distancias, que dado un RoseTree, devuelva las distancias de su raíz a cada una de sus hojas.
c) altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
RoseTree es una hoja, se considera que su altura es 1.
-}
-- I
data RoseTree a = Leaf a | Rose a [RoseTree a]

-- II
foldRT :: (a->b) -> (a->[b]->b)-> RoseTree a -> b
foldRT fLeaf fRose (Leaf a) = fLeaf a
foldRT fLeaf fRose (Rose a ls) = fRose a (map (\e -> foldRT fLeaf fRose e) ls)


-- III
hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\v -> [v]) (\_ recList -> concat recList)

distanciasRT :: RoseTree a -> [Int]
distanciasRT = foldRT (const [1]) (\_ recList -> map (+ 1) (concat recList))

alturaRT :: RoseTree a -> Int
alturaRT = foldRT (const 1) (\_ recList -> (foldr max 0 recList)+1)
