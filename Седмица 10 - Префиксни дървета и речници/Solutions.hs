import Prelude hiding (lookup)
import qualified Prelude as P
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type AssocList k v = [(k, v)]

class Dictionary d where
  empty :: d a
  singleton :: String -> a -> d a
  lookup :: String -> d a -> Maybe a
  insert :: String -> a -> d a -> d a
  toList :: d a -> AssocList String a

newtype Assoc k v = Assoc { getAssoc :: AssocList k v}

insertA :: Eq k => k -> v -> AssocList k v -> AssocList k v
insertA key value l =
  let existing = P.lookup key l
  in case existing of
    Nothing -> (key, value) : l
    (Just _) -> map (\(k, v) -> (k, if k == key then value else v)) l

instance Dictionary (Assoc String) where
  empty :: Assoc String a
  empty = Assoc []

  singleton :: String -> a -> Assoc String a
  singleton key value = Assoc [(key, value)]

  lookup :: String -> Assoc String a -> Maybe a
  lookup key (Assoc l) = P.lookup key l

  insert :: String -> a -> Assoc String a -> Assoc String a
  insert key value (Assoc l) = Assoc $ insertA key value l

  toList :: Assoc String a -> AssocList String a
  toList (Assoc l) = l

data Trie a = Trie (Maybe a) (AssocList Char (Trie a))

instance Show a => Show (Trie a) where
  show :: Show a => Trie a -> String
  show = showTrie 0
    where
      indentStr :: Int -> String
      indentStr n = replicate (n * 2) ' '

      showChild :: Show a => Int -> (Char, Trie a) -> String
      showChild indent (c, sub) =
        indentStr indent ++ "└─ " ++ [c] ++ ":\n" ++ showTrie (indent + 1) sub

      showTrie :: Show a => Int -> Trie a -> String
      showTrie indent (Trie v ch) =
        indentStr indent ++ maybe "_" show v ++ "\n" ++ concatMap (showChild (indent + 1)) ch

instance Dictionary Trie where
  empty :: Trie a
  empty = Trie Nothing []

  singleton :: String -> a -> Trie a
  singleton "" value = Trie (Just value) []
  singleton (c:cs) value = Trie Nothing [(c, singleton cs value)]

  lookup :: String -> Trie a -> Maybe a
  lookup "" (Trie value _) = value
  lookup (c:cs) (Trie _ children) = P.lookup c children >>= lookup cs
    -- do 
    --   child <- P.lookup c children
    --   lookup cs child

  insert :: String -> a -> Trie a -> Trie a
  insert "" value (Trie _ children) = Trie (Just value) children
  insert (c:cs) value (Trie root children) = 
    let trie = P.lookup c children
    in Trie root $ insertA c (insert cs value $ fromMaybe empty trie) children

  toList :: Trie a -> AssocList String a
  toList = toPrefixList ""
    where
      toPrefixList :: String -> Trie a -> AssocList String a
      toPrefixList prefix (Trie value children) =
        let rest = concatMap (\(c, trie) -> toPrefixList (prefix ++ [c]) trie) children
        in maybe rest (\root -> (prefix, root) : rest) value

fromList :: Assoc String a -> Trie a
fromList l = foldr (uncurry insert) empty $ getAssoc l

allWords :: Trie a -> [String]
allWords = map fst . toList

groupBy :: Dictionary d => (a -> String) -> [a] -> d [a]
groupBy f = foldr (updateGroups f) empty
  where
    updateGroups :: Dictionary d => (a -> String) -> a -> d [a] -> d [a]
    updateGroups f x dict = 
      let key = f x
          currentGroup = lookup key dict
      in maybe (insert key [x] dict) (\group -> insert key (x:group) dict) currentGroup

instance Dictionary (M.Map String) where  
  empty :: M.Map String a
  empty = M.empty
  singleton :: String -> a -> M.Map String a
  singleton = M.singleton
  lookup :: String -> M.Map String a -> Maybe a
  lookup = M.lookup
  insert :: String -> a -> M.Map String a -> M.Map String a
  insert = M.insert
  toList :: M.Map String a -> AssocList String a
  toList = M.toList

groupBy' :: Ord k => (v -> k) -> [v] -> M.Map k [v]
groupBy' f = foldr (updateGroups f) M.empty
  where
    updateGroups :: Ord k => (v -> k) -> v -> M.Map k [v] -> M.Map k [v]
    updateGroups f x dict = 
      let key = f x
          currentGroup = M.lookup key dict
      in maybe (M.insert key [x] dict) (\group -> M.insert key (x:group) dict) currentGroup
