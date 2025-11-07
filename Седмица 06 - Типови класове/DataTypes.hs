module DataTypes where

import Prelude hiding (Maybe, Just, Nothing, maybe, Either, Left, Right, either, iterate)

data Maybe a = Just a | Nothing deriving Show

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

fromJust :: Maybe a -> a
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just x) = f x

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f

data Either a b = Left a | Right b deriving Show

fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right x) = g x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

rights :: [Either a b] -> [b]
rights = mapMaybe eitherToMaybe

data NonEmpty a = a :| [a] deriving Show

fromList :: [a] -> Maybe (NonEmpty a)
fromList [] = Nothing
fromList (x:xs) = Just $ x :| xs

foldr1NE :: (a -> a -> a) -> NonEmpty a -> a
foldr1NE op (x:|xs) = foldr1 op (x:xs)

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

iterateNE :: (a -> a) -> a -> NonEmpty a
iterateNE f x = x :| iterate f (f x)

data Nat = Zero | Succ Nat deriving Show

plus :: Nat -> Nat -> Nat
plus Zero x = x
plus (Succ x) y = Succ $ plus x y

multiply :: Nat -> Nat -> Nat
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply (Succ Zero) x = x
multiply (Succ x) y = plus y $ multiply x y

fromInt :: Int -> Maybe Nat
fromInt n
  | n < 0 = Nothing
  | n == 0 = Just Zero
  | otherwise = Just $ Succ $ fromJust $ fromInt $ n - 1

five :: Nat
five = Succ $ Succ $ Succ $ Succ $ Succ Zero

three :: Nat
three = Succ $ Succ $ Succ Zero

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x

data Chain a = Empty | Singleton a | Append (Chain a) (Chain a)

(<:) :: a -> Chain a -> Chain a
x <: Empty = Singleton x
x <: Singleton y = Append (Singleton x) (Singleton y)
x <: c@(Append c1 c2) = Append (Singleton x) c

