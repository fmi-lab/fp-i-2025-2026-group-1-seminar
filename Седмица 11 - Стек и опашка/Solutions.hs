import Stack ( Stack, emptyS, push, pop, peek, popUnsafe )
import Queue
import Data.Maybe (fromJust, fromMaybe)

test :: Maybe Int
test = do
  (n, s1) <- pop $ push 7 emptyS
  (p, s2) <- pop s1
  return p

binaries :: [String]
binaries = "0" : binariesQueue (enqueue "1" emptyQ)
  where
    binariesQueue :: Queue String -> [String]
    binariesQueue queue = fromJust $ do
      (current, updatedQueue) <- dequeue queue
      return $ current : binariesQueue (enqueue (current ++ "1") $ enqueue (current ++ "0") updatedQueue)

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

bfs :: BinaryTree a -> [a]
bfs tree = bfsHelper $ enqueue tree emptyQ
  where
    bfsHelper :: Queue (BinaryTree a) -> [a]
    bfsHelper queue = fromMaybe [] $ do
        (current, updatedQueue) <- dequeue queue
        return $ case current of
          Empty -> bfsHelper updatedQueue
          (Node root left right) -> root : bfsHelper (enqueue right $ enqueue left updatedQueue)

testTree :: BinaryTree Int
testTree = Node 5
                     (Node 1
                           (Node 4
                                 Empty
                                 (Node 13 Empty Empty))
                           (Node 3 Empty Empty))
                     (Node 8
                           (Node 0
                                 (Node 10 Empty Empty)
                                 (Node 9 Empty Empty))
                           (Node 11 Empty Empty))

toList :: Stack a -> [a]
toList stack = fromMaybe [] $ do
  (current, updatedStack) <- pop stack
  return $ current : toList updatedStack 

nextHigher :: [Int] -> [Int]
nextHigher = nextHigherHelper emptyS emptyS . reverse
  where
    nextHigherHelper :: Stack Int -> Stack Int -> [Int] -> [Int]
    nextHigherHelper  _ result [] = toList result
    nextHigherHelper stack result (x:xs) = 
      let updatedStack = untilMin stack x
          greater = fromMaybe (-1) $ peek updatedStack
      in nextHigherHelper (push x updatedStack) (push greater result) xs

    untilMin :: Stack Int -> Int -> Stack Int
    untilMin stack x = fromMaybe emptyS $ do
      y <- peek stack
      return $ if y < x 
               then untilMin (popUnsafe stack) x
               else stack