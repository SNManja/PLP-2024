type Procesador a b = a -> [b]

-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.

-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines

--Ejercicio 1
-- ! Consultar. Todo procesador tiene que devolver una lista
-- ! Ahora, en los casos que el enunciado solo pide cosas como el Id que es un solo elemento
-- ! Esta bien devolverlo en una lista y ya?
procVacio :: Procesador a b
procVacio = (\x -> [])

procId :: Procesador a a
procId = (\x -> [x])

procCola :: Procesador [a] a
procCola = (\(x:xs) -> xs)

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose a hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT (Tern v h1 h2 h3) = [h1,h2,h3] 
procHijosAT Nil = []

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo r hijos) = [r]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo r hijos) = hijos

-- ! Eliminar esto, es para pensar noma
-- Esto seria cNil -> cTer -> b
-- cNil tiene mismo tipo que la rta
-- cTer tiene tipo (nodos de arbol a -> recursion -> recursion -> recursion)
foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cTer arbol = 
    case arbol of
        Nil -> cNil
        Tern v h1 h2 h3 -> cTer v (rec h1) (rec h2) (rec h3)
    where rec = foldAT cNil cTer


-- ! No parece tener un caso base fijo, consultar. Creo q esta bien igual
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose v hijos) = cRose v (map (\hijo -> rec hijo) hijos)
    where rec = foldRose cRose 

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cTrie (TrieNodo r (hijos)) = cTrie r (map (\(caracter,hijo) -> (caracter, rec hijo)) hijos)
    where rec = foldTrie cTrie


-- Ejercicio 3

unoxuno :: Procesador [a] [a]
unoxuno = map (\x -> [x])


-- ! No devuelve el caso de lista vacia
sufijos :: Procesador [a] [a]
sufijos l = foldr (\x rec -> [(drop (length rec) l)] ++ rec) [] l 

-- Ejercicio 4
-- TODO: Testing
preorder :: AT a -> [a]
preorder = foldAT [] (\v h1 h2 h3 -> v : h1 ++ h2 ++ h3)

postorder :: AT a -> [a]
postorder = foldAT [] (\v h1 h2 h3 ->  h1 ++ h2 ++ h3 ++ [v])

inorder :: AT a -> [a]
inorder = foldAT [] (\v h1 h2 h3 -> h1 ++ h2 ++ [v] ++ h3)

exampleTernTree :: AT Int
exampleTernTree = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil))
                        (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil))
                        (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

-- Ejercicio 5

rt :: RoseTree Int
rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 

{-
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRose (Rose v hijos) = cRose v (map (\hijo -> rec hijo) hijos)
    where rec = foldRose cRose 
-}
-- TODO test
preorderRose :: Procesador (RoseTree a) a
preorderRose =  foldRose (\v hijos -> v : concat hijos)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\v hijos -> 
    if null hijos then [v]
        else concat hijos )

-- ! Esta bien? Testear
ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\v caminos ->
    if null caminos then [[v]]
        else map (\camino -> v : concat camino) caminos)


-- Ejercicio 6
-- TODO No funciona. No devuelve todos los caminos, solo los q llegan al final
-- ! Talvez con esto puedo hacer el 7. Leer bien el enunciado
caminos :: Trie a -> [[Char]]
caminos = foldTrie (\m caminos->
    if null caminos then []
    else 
         (map (\(c, camino) -> c:(concat camino)) (caminos)))


exampleTrie :: Trie Bool
exampleTrie = TrieNodo Nothing [('a', TrieNodo (Just True) []),('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]



palabras :: Trie a -> [[Char]]
palabras = foldTrie (\m caminos->
    if null caminos || isNothing m then []
    else 
         (map (\(c, camino) -> c:(concat camino)) (caminos)))
