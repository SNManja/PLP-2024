import System.Win32 (xBUTTON1, cOLOR_ACTIVEBORDER)
-- implementar 

elem3 :: Eq a => a -> [a] -> Bool
elem3 e = foldr (\x acc -> (x == e) || acc) False 

sumaAlternada :: Num a => [a] -> a
sumaAlternada = foldr (\x acc -> x - acc) 0

{-
[1,2,3,4] -> 1-2+3-4

[2,3,4] -> 2-3+4

-}
recr :: (a->[a]->b->b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)


takeRecr :: Int -> [a] -> [a]
takeRecr n (y:ys) = recr (\x xs acc -> if length (x:xs) >= (length (y:ys) - n) then x : acc else acc) [] (y:ys)

takeFoldr :: Int -> [a] -> [a]
takeFoldr e (x:xs) = foldr (\n acc -> if(length (n:acc) < e) then n:acc else acc) [] (x:xs)

-- ! Para analisar
takeDelProfe :: [a] -> Int -> [a]
takeDelProfe = foldr (\x rec -> \n -> if n == 0 then [] else x : rec (n-1))
            (const [])

sacarUna :: Eq => a -> [a] -> [a]
sacarUna e = recr (\x xs rec -> if x ==e then xs else x : rec) []

-- ! Consultar que es recursion global

pares :: [(Int,Int)]
pares = [(x,y) | x <- [1..], y <- [1..]]


data Polinomio  a = X 
                    | Cte a
                    | Suma (Polinomio a) (Polinomio a)
                    | Prod (Polinomio a) (Polinomio a)                    


evaluar :: Num a => a -> Polinomio a -> a
evaluar x pol =  case pol of 
                    X -> x
                    Cte c -> c
                    Suma p q -> rec p + rec q 
                    Prod p q -> rec p * rec q

foldPoli :: b -> (a-> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio b
foldPoli cX cCte cSuma cProd pol = case pol of 
                                        X -> cX 
                                        Cte c -> (cCte c)
                                        Suma p q -> cSuma (rec p) (rec q)
                                        Prod p q -> cProd (rec p) (rec q)
                                        where rec = foldPoli cX cCte cSuma cProd


evaluar2 :: Num a => a -> Polinomio a -> a
evaluar2 x = foldPoli x id (+) (*)


data RoseTree a = Rose a [RoseTree a ]

miRT =  Rose 1 [ Rose 2 [], Rose 3 [Rose 4 [], Rose 5 [], Rose 6 []]]

foldRose :: (a -> [b] -> b)  -> RoseTree a ->  RoseTree b
foldRose cRose (Rose n hijos) = cRose n (map rec hijos) 
                             where rec = foldRose cRose

roseSize :: (RoseTree a) -> Int
roseSize = foldRose (\n rec -> 1 + sum rec)

cantHojas :: RoseTree a -> Int
cantHojas = foldRose (\n recs -> if null recs then 1 else sum rec) 

