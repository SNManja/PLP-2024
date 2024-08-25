/*
Dar el tipo y describir funcionamiento de las siguientes funciones del modulo prelude de Haskell

- null: 
Checkea si al estructura esta vacia
null :: Maybe a -> bool
Este lo busque

- head: 
Devuelve primer elemento de una lista
head :: [a] -> a

- tail:
Devuelve la lista sin primer elemento
tail :: [a] -> [a]

- init:
Devuelve todos los elementos de una lista menos el ultimo
init :: [a] -> [a]


- last:
Devuelve el ultimo elemento de una secuencia
last :: [a] -> a

- take:
Aplicado a una lista xs, devuelve el prefijo de xs de largo n
take :: Int -> [a] -> [a]

- drop: 
Suelta todos los elementos de una secuencia desde el elemento n
drop :: [a] -> b -> a

- (++):
Concatena dos secuencias
(++) :: [a] -> [a] -> [a]

- concat
Teniendo una lista de 2D, la aplana
concat :: Foldable t => t [a] -> [a] 
Este lo busque

- reverse
Da vuelta una secuencia
reverse :: [a] -> [a]

- elem
Verifica si un elemento esta o no en una lista
elem :: a -> [a] -> bool



Consulta:
- Que es un Foldable? Que representa => ? y t [a] como tipo que es?

Foldable parece ser una clase de estructuras de datos que pueden ser reducidas a un elemento a la vez.
Esto puede ser un arbol o una lista

=> es una limitacion de tipo de clase, [a] tiene que ser tipo t

*/