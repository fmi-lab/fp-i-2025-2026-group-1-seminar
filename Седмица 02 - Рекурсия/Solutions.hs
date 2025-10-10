sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n - 1)

-- >>> sum' 10
-- 55

countDigits :: Int -> Int
countDigits n
  | n < 0 = countDigits $ abs n
  | n >= 0 && n < 10 = 1
  | otherwise = 1 + countDigits (n `div` 10)

-- >>> countDigits 47526
-- 5

-- >>> countDigits (-4782)
-- 4

divisorsSum :: Int -> Int
divisorsSum = for 1
  where for :: Int -> Int -> Int
        for i n
          | i == n = n
          | n `rem` i == 0 = i + for (i + 1) n
          | otherwise = for (i + 1) n

toBinary :: Int -> Int
toBinary 0 = 0
toBinary n = n `rem` 2 + 10 * toBinary (n `div` 2)

evalPolynomial :: Double -> Double -> Double -> Double
evalPolynomial = evalPolynomialIter 0
  where evalPolynomialIter :: Double -> Double -> Double -> Double -> Double
        evalPolynomialIter result x a b
          | a > b = result
          | otherwise = evalPolynomialIter (result * x + a) x (a + 1) b

fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacciIter 3 1 1
  where fibonacciIter :: Int -> Int -> Int -> Int
        fibonacciIter i f1 f2
          | i == n = f1 + f2
          | otherwise = fibonacciIter (i + 1) (f1 + f2) f1

palindrome :: Int -> Bool
palindrome n = n == reverse' n 0
  where reverse' :: Int -> Int -> Int
        reverse' n result
          | n == 0 = result
          | otherwise = reverse' (n `div` 10) (result * 10 + n `rem` 10)

prime :: Int -> Bool
prime 1 = False
prime n = for 2 n
  where for :: Int -> Int -> Bool
        for i n = fromIntegral i >= sqrt (fromIntegral n) || n `rem` i /= 0 && for (i + 1) n
