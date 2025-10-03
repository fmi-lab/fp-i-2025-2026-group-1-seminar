sign :: Double -> String
sign n
  | n > 0 = "positive"
  | n < 0 = "negative"
  | otherwise = "zero"

roots :: Double -> Double -> Double -> Int
roots a b c
  | discriminant > 0 = 2
  | discriminant == 0 = 1
  | otherwise = 0
  where discriminant :: Double
        discriminant = b ^^ 2 - 4 * a * c

superNumber :: Double -> Double -> Double -> Double
superNumber a b c = 
  let min' = min a (min b c)
      max' = max a (max b c)
      middle = a + b + c - min' - max'
  in min' * max' + middle

modulus :: (Double, Double) -> Double
modulus (r, i) = sqrt (r ^^ 2 + i ^^ 2)

compute :: (Int, Int, Double) -> Double
compute (0, _, _) = 0
compute (a, b, c) = c / fromIntegral a + fromIntegral b

(~=) :: Double -> Double -> Bool
a ~= b = let epsilon = 1e-6
  in abs (a - b) < epsilon

factorial :: Int -> Int
factorial 0 = 1
factorial a = a * factorial (a - 1)

factorial' :: Int -> Int
factorial' a
  | a == 0 = 1
  | a > 0 = a * factorial' (a - 1)

factorial'' :: Int -> Int
factorial'' a = if a == 0 then 1 else a * factorial'' (a - 1)