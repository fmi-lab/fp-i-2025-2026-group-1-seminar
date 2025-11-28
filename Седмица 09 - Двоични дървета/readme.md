# Седмица 09 - Двоични дървета

```hs
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

{-
        5
      /   \
     1    8
    / \  / \
   4  3 0  11
   \   / \
   13 10 9
-}
```

## Задача 01 - Височина на дърво
Напишете функция `height`, която намира височината на двоично дърво.

### Пример:
```hs
ghci> height testTree -- 4
```

## Задача 02 - Брой листа
Напишете функция `countLeafs`, която намира броя на листата на двоично дърво

### Пример:
```hs
ghci> countLeafs testTree -- 5
```

## Задача 03 - Ротации
Напишете функции, които правят лява и дясна ротация на двоично дърво.

### Пример:
```hs
ghci> rotateLeft testTree -- Node 8 (Node 5 (Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 3 Empty Empty)) (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty))) (Node 11 Empty Empty)
ghci> rotateRight testTree -- Node 1 (Node 4 Empty (Node 13 Empty Empty)) (Node 5 (Node 3 Empty Empty) (Node 8 (Node 0 (Node 10 Empty Empty) (Node 9 Empty Empty)) (Node 11 Empty Empty)))
```

## Задача 04 - Инстанции
Направете инстанции на класовете `Functor` и `Foldable` за типа `BinaryTree`. За класа `Foldable` направете обхождането да е по стратегията "ляво-корен-дясно".

### Пример:
```hs
ghci> (^ 2) <$> testTree
{-
Node 25 
     (Node 1 
           (Node 16 
                 Empty 
                 (Node 169 Empty Empty)) 
           (Node 9 Empty Empty)) 
     (Node 64 
           (Node 0 
                 (Node 100 Empty Empty) 
                 (Node 81 Empty Empty)) 
           (Node 121 Empty Empty))
-}

ghci> foldr ((++) . (++ " ") . show) "" testTree -- 4 13 1 3 5 10 0 9 8 11
ghci> sum testTree -- 64
ghci> 10 `elem` testTree -- True
ghci> length testTree -- 10
ghci> maximum testTree -- 13
```

### Бонус:
Направете инстанция на класа `Traversable` за типа `BinaryTree`.

### Пример:
```hs
ghci> sequence $ Just <$> testTree
{-
Just (Node 5 
           (Node 1 
               (Node 4 
                       Empty 
                       (Node 13 Empty Empty)) 
               (Node 3 Empty Empty)) 
           (Node 8 
               (Node 0 
                       (Node 10 Empty Empty) 
                       (Node 9 Empty Empty)) 
               (Node 11 Empty Empty)))
-}
```

## Задача 05 - Ниво на дърво
Напишете функция `level`, която връща списък от елементите на двоично дърво, които са на едно и също ниво. Приемаме, че първото ниво е 0.

### Пример:
```hs
ghci> level testTree 2 -- [4, 3, 0, 11]
```

## Задача 06 - Промяна и изтриване на елемент в двоично дърво
Напишете функции `update` и `remove`, които приемат позиция на елемент в двоично дърво и съответно променят стойността в този възел на нова подадена стойност и премахват поддървото с корен равен на този възел. За позицията използвайте типа `Trace` от лекции.

### Пример:
```hs
data Directrion = L | R
   deriving (Eq, Show)

type Trace = [Directrion]

ghci> update testTree [R,L] 7
{- 
Node 5 
     (Node 1 
           (Node 4 
                 Empty 
                 (Node 13 Empty Empty)) 
           (Node 3 Empty Empty)) 
     (Node 8 
           (Node 7 
                 (Node 10 Empty Empty) 
                 (Node 9 Empty Empty)) 
           (Node 11 Empty Empty))
-}

ghci> remove testTree [R,L]
{-
Node 5 
     (Node 1 
           (Node 4 
                 Empty 
                 (Node 13 Empty Empty)) 
           (Node 3 Empty Empty)) 
     (Node 8 
           Empty 
           (Node 11 Empty Empty))
-}
```

## Задача 07 - Огледално дърво
Напишете функция `mirror`, която проверява дали двоично дърво е огледално. Огледално дърво наричаме такова дърво, на което дясното поддърво има обратно симетрична структура спрямо лявото.

### Пример:
```hs
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

{-
        4
       / \         
      1   6
     /\   /\     
    7  2 0  8
    \      / 
     10   11 
-}

ghci> mirror mirrorTree -- True
ghci> mirror testTree -- False
```

## Задача 08 - Път от корен до листо
Напишете функция `paths`, която по подадено двоично дърво, връща списък от всички пътища от корена до някое листо на дървото.

### Пример:
```hs
ghci> paths testTree -- [[5,1,4],[5,1,4,13],[5,1,3],[5,8,0,10],[5,8,0,9],[5,8,11]]
```

## Задача 09* - Най-близък общ родител
Напишете функция `lowestCommonAncestor`, която по подадени 2 елемента и двоично дърво намира най-близкия общ родител на двата елемента в дървото. Ако няма такъв, функцията да връща `Nothing`.

### Пример:
```hs
ghci> lowestCommonAncestor 9 11 testTree -- Just 8
ghci> lowestCommonAncestor 9 3 testTree -- Just 5
ghci> lowestCommonAncestor 9 50 testTree -- Nothing
```

## Задача 10 - Двоично наредено дърво
```hs
bst :: BST Integer
bst = BSTNode 3 (BSTNode 1 
                         (BSTNode 0 BSTEmpty BSTEmpty) 
                         (BSTNode 2 BSTEmpty BSTEmpty))
                (BSTNode 5 
                         BSTEmpty 
                         (BSTNode 6 BSTEmpty BSTEmpty))

{-
        3
       / \
      1   5
     / \   \
    0  2   6
-}
```

Напишете следните функции, които използват двоично наредено дърво:

- `search :: Ord t => t -> BST t -> Bool` - търси елемет в двоично наредено дърво
- `insert :: Ord t => t -> BST t -> BST t` - добавя елемент към двоично наредено дърво;
- `remove :: Ord t => t -> BST t -> BST t` - премахва елемент от двоично наредено дърво;
- `kthSmallest :: Ord t => BST t -> Int -> t` - намира k-тия по големина елемент в двоично наредено дърво (Бонус: напишете безопасен вариант с Maybe);
- `rangeSearch :: Ord t => t -> t -> BST t -> [t]` - връща списък от тези елементи от двоично наредено дърво, които са част от подадения интервал;
- `kClosestElements :: Ord a => a -> Int -> BST a -> [a]` - връща списък от първите k елементи, които са най-близки до даден елемент в двоично наредено дърво.

### Примери
```hs
ghci> search 2 bst -- True
ghci> search 7 bst -- False

ghci> insert 5 bst
{-
BSTNode 3 
        (BSTNode 1 
                 (BSTNode 0 BSTEmpty BSTEmpty) 
                 (BSTNode 2 BSTEmpty BSTEmpty)) 
        (BSTNode 5 
                 (BSTNode 4 BSTEmpty BSTEmpty) 
                 (BSTNode 6 BSTEmpty BSTEmpty))
-}

ghci> remove' 2 bst
{-
BSTNode 3 
        (BSTNode 1 
                 (BSTNode 0 BSTEmpty BSTEmpty) 
                 BSTEmpty) 
        (BSTNode 5 
                 BSTEmpty 
                 (BSTNode 6 BSTEmpty BSTEmpty))
-}
ghci> remove' 5 bst
{-
BSTNode 3 
        (BSTNode 1 
                 (BSTNode 0 BSTEmpty BSTEmpty) 
                 (BSTNode 2 BSTEmpty BSTEmpty)) 
        (BSTNode 6 BSTEmpty BSTEmpty)
-}
ghci> remove' 3 bst
{-
BSTNode 5 
        (BSTNode 1 
                 (BSTNode 0 BSTEmpty BSTEmpty) 
                 (BSTNode 2 BSTEmpty BSTEmpty)) (
        BSTNode 6 BSTEmpty BSTEmpty)
-}

ghci> kthSmallest bst 4 -- 5
ghci> rangeSearch 2 5 bst -- [2,3,5]
```

