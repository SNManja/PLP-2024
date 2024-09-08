{-
Dado el tip o AB a del ejercicio 12:
i. Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.

i i. Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árb oles, indica si éstos
tienen la misma forma, independientemente del contenido de sus no dos. Pista: usar evaluación parcial y
recordar el ejercicio 8.
-}

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cLeaf cBin t =  
    case t of
        Nil -> cLeaf
        Bin n1 v n2 -> cBin (rec n1) v (rec n2)
    where rec = foldAB cLeaf cBin   

recrAB :: b -> (b -> AB a -> a -> b -> AB a -> b) -> AB a -> b
recrAB cLeaf cBin t = 
    case t of 
        Nil -> cLeaf
        Bin n1 v n2 -> cBin (rec n1) n1 v (rec n2) n2
    where rec = recrAB cLeaf cBin

cantHojas :: AB a -> Int 
cantHojas t = 1 + foldAB 0 (\n1 v n2 -> n1 + n2 + 1) t 


{-
takeDelProfe :: [a] -> Int -> [a]
takeDelProfe = foldr (\x rec -> \n -> if n == 0 then [] else x : rec (n-1))
            (const [])
-}
mismaEstructura :: AB a -> AB a -> Bool
mismaEstructura t1 t2 = foldAB True (\n1 -> n2 -> case (n1,n2) of
    (Nil,Nil) -> True
    (Bin _ _ _, Nil) -> False
    (Nil, Bin _ _ _) -> False
    (Bin _ v1 _, Bin _ v2 _) -> (v1 && v2) ) t1 t2


-- ! Ayuda, no comprendo si puedo evaluar en recurrencia dentro de un AB
-- ! Ni tampoco como dar uso de la currificacion en el AB como en el ejemplo del take
-- ! No tegnoi idea de nada la verdad, perdidisimo