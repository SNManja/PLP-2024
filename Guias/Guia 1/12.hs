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

esNil :: AB a -> Bool
esNil t = (foldAB 1 (\n1 v n2 -> n1 + n2) t) == 1

cantNodos :: AB a -> Integer
cantNodos t = foldAB  0 (\n1 v n2 -> n1 + n2 + 1) t

mejorSegun :: (a -> a -> Bool) -> AB a -> Maybe a
mejorSegun f  = foldAB Nothing (\n1 v n2 ->     case (n1, n2) of
                                                (Nothing, Nothing) -> Just v
                                                (Just n1', Nothing) -> Just (if f n1' v then n1' else v)
                                                (Nothing, Just n2') -> Just (if f v n2' then v else n2')
                                                (Just n1', Just n2') -> Just (if f n1' n2' then n1' else n2'))

esABB :: Ord a => AB a -> Bool
esABB t = recrAB True (\n1 t1 v n2 t2 ->
    case (t1,t2) of
        (Nil,Nil) -> True 
        (Bin _ vt1 _, Nil) -> (n1 && vt1 <= v) 
        (Nil, Bin _ vt2 _) -> (n2 && v <= vt2)
        (Bin _ vt1 _, Bin _ vt2 _)  -> n1 && n2 && vt1 <= v && vt2 >= v) t

arbolNulo :: AB a
arbolNulo = Nil

arbol1 :: AB Integer
arbol1 = Bin (Nil) 1 (Nil)

arbol2 :: AB Integer
arbol2 = Bin (Bin (Bin Nil 3 Nil) 2 (Bin Nil 4 Nil)) 1 (Bin Nil 6 (Bin Nil 5 Nil))
