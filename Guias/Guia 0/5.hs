data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool 
negacionAB Nil _ _ _ = Nil
negacionAB Bin left x right = (negacionAB left) (not x) (negacionAB right) 

productoAB :: AB Int -> Int
productoAB Nil _ _ _= 1
productoAB Bin left x right = x * productoAB left * productoAB right

-- ! Ayuda