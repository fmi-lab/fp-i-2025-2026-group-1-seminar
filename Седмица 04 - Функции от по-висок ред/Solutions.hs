import Prelude hiding (const, (.), foldr, foldl, map, takeWhile, foldr1)

const :: a -> (b -> a)
const x = \ y -> x

const' :: a -> b -> a
const' x _ = x

(.) ::  (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g $ f x

compose ::  (b -> c) -> (a -> b) -> a -> c
compose g f x = g $ f x

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on g f x y = g (f x) (f y)

composeN :: (a -> a) -> Int -> (a -> a)
composeN f 0 = id
composeN f n = f . composeN f (n - 1)

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

liftB :: (Bool -> Bool -> Bool) -> (a -> Bool) -> (a -> Bool) -> a -> Bool
liftB f p1 p2 x = f (p1 x) (p2 x)

implies :: (a -> Bool) -> (a -> Bool) -> a -> Bool
implies = liftB (==>)

derive :: (Double -> Double) -> Double -> Double
derive f a = let h = 1e-6
  in (f (a + h) - f a) / h

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x:xs) = op x $ foldr op nv xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ nv [] = nv
foldl op nv (x:xs) = foldl op (op nv x) xs

map :: (a -> b) -> [a] -> [b]
-- map f = foldr ((:) . f) []
map f = foldr (\current result -> f current : result) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred = foldr (\current result -> if pred current then current : result else []) []

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 op (x:xs) = foldr op x xs

any :: (a -> Bool) -> [a] -> Bool
any pred = foldl (\result current -> pred current || result) False

quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) =
  let pivot = x
      -- less = filter (`cmp` pivot) xs
      less = [y | y <- xs, y `cmp` x]
      greater = filter (not . (`cmp` pivot)) xs
  in quickSortBy cmp less ++ [pivot] ++ quickSortBy cmp greater

rationals :: [(Int, Int)]
rationals = [(x - y, y) | x <- [1..], y <- [1..x]]
-- rationals = [(x, y) | n <- [1..], x <- [0..n], y <- [1..n], x + y == n]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = let s = subsets xs
  in map (x:) s ++ s