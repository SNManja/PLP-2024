foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f z 1 = z
foldNat f z n = f z (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia n p = foldNat (\x rec -> n * rec) n p

