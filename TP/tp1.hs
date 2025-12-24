import Data.Maybe
import Test.HUnit
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
-- ("", True)
-- ("a", True)
-- ("b", -)
-- ("ba", True)
-- ("bd", - )
-- ("c", True)

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

-- Ejercicio 2
-- En estos casos, sí se puede usar recursión explcita.

-- valor inicial ->  funcion de mapeo          -> arbol ternario -> retorno
--                  (v -> h1 -> h2 -> h3 -> r)
foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cTer arbol = 
    case arbol of
        Nil -> cNil
        Tern v h1 h2 h3 -> cTer v (rec h1) (rec h2) (rec h3)
    where rec = foldAT cNil cTer


-- el caso base es cuando el rose tree no tiene hijos (RoseTree a [])
-- en ese caso map = [] y terminamos con (cRose a [])
-- foldRose :: (a -> [b] -> b) -> RoseTree a -> b
-- foldRose cRose (Rose v hijos) = cRose v (map (\hijo -> rec hijo) hijos)
--     where rec = foldRose cRose
foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose v hijos) = f v (map (\hijo -> rec hijo) hijos)
                            where rec = foldRose f

-- f :: Maybe a -> [(Char, b)] -> b
foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cTrie (TrieNodo r (hijos)) = cTrie r (map (\(caracter, hijo) -> (caracter, rec hijo)) hijos)
    where rec = foldTrie cTrie


-- Ejercicio 3

unoxuno :: Procesador [a] [a]
unoxuno = map (\x -> [x])

sufijos :: Procesador [a] [a]
sufijos = \c -> map (\v -> drop v c) (ls c)
          where ls c = [0..(length c)] -- genera una lista donde cda elem es la cantidad de caracteres a recortar de 'c'
-- sufijos "plp" = ["plp", "lp", "p", ""]

-- Ejercicio 4
{-
    Usamos estos arboles para testear las siguientes funciones.
    atVacio, al pasarlo por cualquier orden tiene que dar [0]
    atPreorder, atPostorder, atInoder al pasarlos por preorder, postorder e inorder
    respectivamente recibimos en los tres casos: [1, 2, 3, 4, 5]
-}
atVacio     = Tern 0 Nil Nil Nil
atPreorder  = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 (Tern 5 Nil Nil Nil) Nil Nil)
atPostorder = Tern 5 (Tern 1 Nil Nil Nil) (Tern 2 Nil Nil Nil) (Tern 4 (Tern 3 Nil Nil Nil) Nil Nil)
atInorder   = Tern 3 (Tern 1 Nil Nil Nil) (Tern 2 Nil Nil Nil) (Tern 5 (Tern 4 Nil Nil Nil) Nil Nil)
exampleTernTree :: AT Int


preorder :: AT a -> [a]
preorder = foldAT [] (\v h1 h2 h3 -> v : h1 ++ h2 ++ h3)
-- preorder atVacio = [0]
-- preorder atPreorder = [1, 2, 3, 4, 5]

postorder :: AT a -> [a]
postorder = foldAT [] (\v h1 h2 h3 ->  h1 ++ h2 ++ h3 ++ [v])
-- postorder atVacio = [0]
-- preorder atPreorder = [1, 2, 3, 4, 5]

inorder :: AT a -> [a]
inorder = foldAT [] (\v h1 h2 h3 -> h1 ++ h2 ++ [v] ++ h3)
-- inorder atVacio = [0]
-- inorder atInorder = [1, 2, 3, 4, 5]

-- Ejercicio 5

-- Rose tree de ejemplo

{-
      a
   /  |  \
  b   c  d
  |      |\
  e      f g
-}

-- TODO test
preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\v hijos -> v : concat hijos)
-- preorderRose rt
--  = "abecdfg"

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\v hijos -> 
    if null hijos then [v]
        else concat hijos )

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\v hs -> 
                        if null hs then [[v]]
                        else map (\h -> v : h) (concat hs)
                    )

-- Ejercicio 6

ejTrie :: Trie Bool
ejTrie = TrieNodo Nothing [
        ('a', TrieNodo (Just True) []),
        ('b', TrieNodo Nothing [
            ('a', TrieNodo (Just True) [
                ('d', TrieNodo Nothing [])
            ])
        ]), 
        ('c', TrieNodo (Just True) [])
    ]

ejTrie2 = TrieNodo (Just True) []
-- caminos ejTrie ⇝ ["", "a", "b", "ba", "bad", "c"]

-- caminos :: Trie a -> [String]
-- f :: Maybe a -> hs -> [String]
-- g :: hs -> [[String]]
-- hs = [( Char, [String] )]
caminos = foldTrie f
          where f v hs = ("":concat (g hs)) -- descartar valor del nodo, aplanar caminos y añadir camino root
                g hs = (map ( \(k,cs) -> (map (prepend k) cs)) hs) -- a todos los caminos de los hijos le añado la clave
                prepend k s = k : s

-- Esto es solo una version modificada de `caminos`,
-- la unica diferencia es que añadimos el camino actual <-> no es Nothing
palabras = foldTrie f
          where f v hs = (concat (g hs)) ++ (este v)
                g hs = (map ( \(k,cs) -> (map (prepend k) cs)) hs)
                prepend k s = k : s
                este v = if isNothing v then [] else [""]

ifProc :: (a -> Bool) -> Procesador a  b -> Procesador a b -> Procesador a b
ifProc f p1 p2 = (\e -> if (f e) then (p1 e) else (p2 e))

(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) p1 p2 = (\e -> (p1 e) ++ (p2 e))

(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p2 p1 = (\e -> concatMap (\i -> (p2 i)) (p1 e))



-- Para el 9 ver las imagenes adjuntas

ejTrie :: Trie Bool
ejTrie = TrieNodo Nothing [
        ('a', TrieNodo (Just True) []),
        ('b', TrieNodo Nothing [
            ('a', TrieNodo (Just True) [
                ('d', TrieNodo Nothing [])
            ])
        ]), 
        ('c', TrieNodo (Just True) [])
    ]

ejTrie2 = TrieNodo (Just True) []
-- caminos ejTrie ⇝ ["", "a", "b", "ba", "bad", "c"]

-- caminos :: Trie a -> [String]
-- f :: Maybe a -> hs -> [String]
-- g :: hs -> [[String]]
-- hs = [( Char, [String] )]

{-Tests-}
exRoseTree :: RoseTree Char
exRoseTree = Rose 'a' [
        Rose 'b' [
            Rose 'e' []
        ],
        Rose 'c' [],
        Rose 'd' [
            Rose 'f' [],
            Rose 'g' [],
            Rose 'h' []
        ]
    ]

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  procVacio 1  -- Test 1. Cualquier valor, devuelve lista vacia          
    ~=? []                                                              
  ,
  procId 1  -- ! Checkear que este bien -- Test 1. Cualquier valor devuelve id
    ~=? 1,
  procCola [] -- Test 1. Lista Vacia
    ~=? [],
  procCola [1] -- Test 2. Lista tama;o 1
    ~=? [],
  procCola [1,2,3] -- Test 3. Lista tama;o >= 2
    ~=? [2,3],
  procHijosRose Rose 'a' [] -- Test 1. Rose sin hijos
    ~=? [],
  procHijosRose exRoseTree -- Test 2. Rose con hijos
    ~=? [Rose 'b' [Rose 'e' []],Rose 'c' [],Rose 'd' [Rose 'f' [],Rose 'g' []]], 
  procHijosAT Nil -- Test 1. AT Nil
    ~=? [],
  procHijosAT Tern 1 Nil Nil Nil  -- Test 2. AT Tern de Nil
    ~=? [Nil,Nil,Nil],
  procHijosAT Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)  -- Test 2. AT Tern
    ~=? [(Tern 2 Nil Nil Nil),(Tern 3 Nil Nil Nil),(Tern 4 Nil Nil Nil)], 
  procRaizTrie ejTrie -- Test 1. Nodo raiz Nothing
    ~=? Nothing, 
  procRaizTrie ejTrie2 -- Test 2. Nodo raiz Just a
    ~=? (Just True), 
  procSubTries ejTrie2 -- Test 1. Sin descendientes
    ~=? [], 
  procSubTries ejTrie -- Con descendientes
    ~=? [
        ('a', TrieNodo (Just True) []),
        ('b', TrieNodo Nothing [
            ('a', TrieNodo (Just True) [
                ('d', TrieNodo Nothing [])
            ])
        ]), 
        ('c', TrieNodo (Just True) [])
    ]                                                            
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  (0,0)       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  unoxuno []      -- Caso de test 1 - Caso vacio
    ~=? [],            -- Caso de test 1 - resultado esperado
  unoxuno [1,2,3] -- Caso de test 2 - Caso no vacio
    ~=? [[1],[2],[3]],  -- Caso de test 2 - resultado esperado
  sufijos []      -- Caso de test 1 - Caso vacio
    ~=? [""],        -- Caso de test 1 - resultado esperado
  sufijos ["plp"]      -- Caso de test 1 - Caso no vacio
    ~=? ["plp","lp","p",""]  -- Caso de test 2 - resultado esperado
  ]

debalancedTernTree = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil)
                            (Tern 7 Nil Nil Nil)
                            Nil)
                    (Nil)
                    (Tern 10 Nil Nil Nil)
exampleTernTree = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil))
                        (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil))
                        (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

testsEj4 = test [ -- Casos de test para el ejercicio 4
  postorder Nil       -- Caso de test 1 - caso vacio
    ~=? [],                             -- Caso de test 1 - resultado esperado
  preorder Nil       -- Caso de test 1 - caso vacio
    ~=? [],                             -- Caso de test 1 - resultado esperado
  inorder Nil       -- Caso de test 1 - caso vacio
    ~=? [],                             -- Caso de test 1 - resultado esperado
  postorder debalancedTernTree       -- Caso de test 2 - arbol desbalanceado
    ~=? [16,1,9,7,10],    
  preorder debalancedTernTree     -- Caso de test 2 - arbol desbalanceado
    ~=? [9,7,1,10,16],   
  inorder debalancedTernTree       -- Caso de test 2 - arbol desbalanceado
    ~=? [9,7,1,16,10],        
  postorder exampleTernTree     -- Caso de test 3 - arbol balanceado
    ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4],
  preorder exampleTernTree      -- Caso de test 3 - arbol balanceado
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16],
  inorder exampleTernTree       -- Caso de test 3 - arbol balanceado
    ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4]
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
 --! PUEDEN ESTAR MAL, Revisar bien
  postorderRose exRoseTree       -- Caso de test 1 - expresión a testear
    ~=? ['a','b','e','c','d','f','g','h'],                       -- Caso de test 1 - resultado esperado
  preorder exRoseTree
    ~=? ['e','b','c','f','g','d','a','h']
  inorder exRoseTree
    ~=? ['e','b','c','f','g','d','h','a']
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  ejTrie       -- Caso de test 1 - test de catedra
    ~=? ["", "a", "b", "ba", "bad", "c"]          
  ejTrie2
    ~=? [""]                                  -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  ejTrie        -- Caso de test 1 - expresión a testear
    ~=? ["a", "ba", "c"],                                          -- Caso de test 1 - resultado esperado
  ejTrie2
    ~=? [""]  
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]