inverso :: Float -> Maybe Float 
inverso 0 = Nothing
inverso n = Just ((**) n (-1))

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right y) = if y then 1 else 0

