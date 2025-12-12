module Queue(Queue, emptyQ, enqueue, headQ, dequeue, isEmptyQ, dequeueUnsafe) where

data Queue a = Queue [a] [a] deriving Show

emptyQ :: Queue a
emptyQ = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front rear) = Queue front $ x:rear

headQ :: Queue a -> Maybe a
headQ (Queue [] []) = Nothing
headQ (Queue [] l) = headQ $ Queue (reverse l) []
headQ (Queue (x:_) _) = Just x

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] l) = dequeue $ Queue (reverse l) []
dequeue (Queue (x:xs) rear) = Just (x, Queue xs rear)

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue front rear) = null front && null rear

dequeueUnsafe :: Queue a -> Queue a
dequeueUnsafe (Queue (_:xs) rear) = Queue xs rear