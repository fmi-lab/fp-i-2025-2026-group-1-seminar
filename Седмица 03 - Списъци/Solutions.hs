import Prelude hiding (zip, take, (!!))

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

take :: Int -> [a] -> [a]
take 0 l = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

rotate :: [a] -> Int -> [a]
rotate l n = drop n l ++ take n l

removeEvery :: [a] -> Int -> [a]
removeEvery [] _ = []
removeEvery l 0 = l
removeEvery l n = take (n - 1) l ++ removeEvery (drop n l) n

compress :: Eq a => [a] -> [a]
compress [] = []
compress l@[_] = l
compress (x:y:xs)
  | x == y = compress (x:xs)
  | otherwise = x : compress (y:xs)

insert :: Ord a => [a] -> a -> [a]
insert [] x = [x]
insert l@(x:xs) y
  | y < x = y:l
  | otherwise = x : insert xs y

-- insert' :: Ord a => [a] -> a -> [a]
-- insert' l x = takeWhile (< x) l ++ [x] ++ dropWhile (<= x) l

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert (insertionSort xs) x

partition :: Ord a => [a] -> a -> ([a], [a])
partition [] _ = ([], [])
partition (x:xs) y =
  let (less, greater) = partition xs y
  in if x < y then (x:less, greater) else (less, x:greater)

merge :: Ord a => [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge left@(x:xs) right@(y:ys)
  | x < y = x : merge xs right
  | otherwise = y : merge left ys

nub :: Ord a => [a] -> [a]
nub l = compress $ insertionSort l

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs)
  | x `elem` xs = nub' xs
  | otherwise = x : nub' xs

nub'' :: Eq a => [a] -> [a]
nub'' l = while l []
  where while :: Eq a => [a] -> [a] -> [a]
        while [] result = result
        while (x:xs) result
          | x `elem` result = while xs result
          | otherwise = while xs (x:result)

nub''' :: Eq a => [a] -> [a]
nub''' l = while l []
  where while :: Eq a => [a] -> [a] -> [a]
        while [] _ = []
        while (x:xs) seen
          | x `elem` seen = while xs seen
          | otherwise = x : while xs (x:seen)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf l r@(x:xs) = isPrefixOf l r || isInfixOf l xs
  where isPrefixOf :: Eq a => [a] -> [a] -> Bool
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys