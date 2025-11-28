data BinaryTree t = Empty | Node t (BinaryTree t) (BinaryTree t)
  deriving Show

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

height :: BinaryTree t -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

countLeafs :: BinaryTree t -> Int
countLeafs Empty = 0
countLeafs (Node _ Empty Empty) = 1
countLeafs (Node _ left right) = countLeafs left + countLeafs right

rotateRight :: BinaryTree t -> BinaryTree t
rotateRight Empty = Empty
rotateRight t@(Node _ Empty _) = t
rotateRight (Node a (Node b t1 t2) t3) = Node b t1 (Node a t2 t3)

instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap _ Empty = Empty
  fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

instance Foldable BinaryTree where
  foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldr _ nv Empty = nv
  foldr op nv (Node root left right) = foldr op (op root $ foldr op nv right) left

level :: Int -> BinaryTree t -> [t]
level _ Empty = []
level 0 (Node root _ _) = [root]
level n (Node root left right) = level (n - 1) left ++ level (n - 1) right

data Directrion = L | R
   deriving (Eq, Show)

type Trace = [Directrion]

update :: Trace -> t -> BinaryTree t -> BinaryTree t
update _ _ Empty = Empty
update [] x (Node _ left right) = Node x left right
update (L:xs) x (Node root left right) = Node root (update xs x left) right
update (R:xs) x (Node root left right) = Node root left (update xs x right)

mirror :: BinaryTree t -> Bool
mirror Empty = True
mirror (Node _ left right) = areMirrored left right
  where
    areMirrored :: BinaryTree t -> BinaryTree t -> Bool
    areMirrored Empty Empty = True
    areMirrored Empty _ = False
    areMirrored _ Empty = False
    areMirrored (Node _ l1 r1) (Node _ l2 r2) =
      areMirrored l1 r2 && areMirrored l2 r1

mirrorTree :: BinaryTree Int
mirrorTree = Node 4
                  (Node 1
                        (Node 7
                              Empty
                              (Node 10 Empty Empty))
                        (Node 2 Empty Empty))
                  (Node 6
                        (Node 0 Empty Empty)
                        (Node 8
                              (Node 11 Empty Empty)
                              Empty))

paths :: BinaryTree t -> [[t]]
paths Empty = []
paths (Node root Empty Empty) = [[root]]
paths (Node root left right) = map (root:) $ paths left ++ paths right

data BST t = BSTEmpty | BSTNode t (BST t) (BST t)
  deriving Show

bst :: BST Integer
bst = BSTNode 3 (BSTNode 1
                         (BSTNode 0 BSTEmpty BSTEmpty)
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 5
                         BSTEmpty
                         (BSTNode 6 BSTEmpty BSTEmpty))

instance Foldable BST where
  foldr :: (a -> b -> b) -> b -> BST a -> b
  foldr _ nv BSTEmpty = nv
  foldr op nv (BSTNode root left right) = foldr op (op root $ foldr op nv right) left

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

kthSmallest :: Ord t => BST t -> Int -> Maybe t
kthSmallest tree k
  | let l = length tree in k < 1 || k > l = Nothing
  | otherwise = Just $ toList tree !! (k - 1)

buildBst :: Ord t => [t] -> BST t
buildBst = build . sort
  where
    sort :: Ord t => [t] -> [t]
    sort [] = []
    sort (x:xs) = let less = filter (< x) xs
                      greater = filter (>= x) xs
                  in sort less ++ [x] ++ sort greater

    build :: Ord t => [t] -> BST t
    build [] = BSTEmpty
    build l = let (left, mid:right) = splitHalf l
      in BSTNode mid (build left) (build right)

    splitHalf :: [t] -> ([t], [t])
    splitHalf l = let mid = length l `div` 2
      in splitAt mid l