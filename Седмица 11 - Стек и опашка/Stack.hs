module Stack(Stack, emptyS, push, peek, pop, isEmptyS, popUnsafe) where

newtype Stack a = Stack [a] deriving Show

emptyS :: Stack a
emptyS = Stack []

push :: a -> Stack a -> Stack a
push x (Stack l) = Stack $ x:l

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

isEmptyS :: Stack a -> Bool
isEmptyS (Stack l) = null l

popUnsafe :: Stack a -> Stack a
popUnsafe (Stack (_:xs)) = Stack xs