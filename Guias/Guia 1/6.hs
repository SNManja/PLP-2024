recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- ! Habria que revisar el caso en que agrega al final
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem (x:xs) = recr (\n ns acc -> if (apariciones elem (n:ns) == apariciones elem(x:xs)) && elem == n 
                                            then acc 
                                            else n : acc ) [] (x:xs)
                                            

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e xs = foldr (\n acc -> if n == e then 1 + acc else acc) 0 xs

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem (x:xs) = recr (\n ns acc -> if( elem > n && elem < (head ns)) ||
                                                    (elem == n && apariciones elem (n:ns) == apariciones elem (x:xs)) 
                                                    then  (n : [elem]) ++ acc
                                                    else n: acc  ) [] (x:xs)