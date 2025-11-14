{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}
import Prelude hiding (Maybe, Just, Nothing, maybe, Either, Left, Right, either, Semigroup, Monoid, (<>), mconcat, mempty, mappend, Functor, (<$>), fmap)

import DataTypes

chain :: Chain Int
chain = Append (Append (Singleton 1) (Singleton 2)) (Append (Append (Singleton 3) (Singleton 4)) (Singleton 5))

foldlChain :: (b -> a -> b) -> b -> Chain a -> b
foldlChain op nv chain = foldlChainIter op nv [chain]
  where foldlChainIter :: (b -> a -> b) -> b -> [Chain a] -> b
        foldlChainIter op nv [] = nv
        foldlChainIter op nv (Empty:cs) = foldlChainIter op nv cs
        foldlChainIter op nv ((Singleton x):cs) = foldlChainIter op (op nv x) cs
        foldlChainIter op nv ((Append c1 c2):cs) = foldlChainIter op nv (c1:c2:cs)

data Student = Student {
  fn :: String,
  name :: String,
  email :: String,
  phone :: Maybe String
}

data ValidationError =
  InvalidFnError |
  InvalidNameError |
  InvalidEmailError |
  InvalidPhoneError
  deriving Show

createStudent :: String -> String -> String -> Maybe String -> Either [ValidationError] Student
createStudent fn name email phone = 
  let invalid = catMaybes [validateFn fn, validateName name, validateEmail email, validatePhone phone]
  in if null invalid
     then Right $ Student fn name email phone
     else Left invalid
  where validateFn :: String -> Maybe ValidationError
        validateFn fn
          | length fn == 10 = Nothing
          | otherwise = Just InvalidFnError

        validateName :: String -> Maybe ValidationError
        validateName name
          | length (words name) == 3 = Nothing
          | otherwise = Just InvalidNameError

        validateEmail :: String -> Maybe ValidationError
        validateEmail email
          | let rest = dropWhile (/= '@') email 
            in not (null rest) && '.' `elem` tail rest = Nothing
          | otherwise = Just InvalidEmailError

        validatePhone :: Maybe String -> Maybe ValidationError
        validatePhone = maybe Nothing validate
          where validate :: String -> Maybe ValidationError
                validate phone
                  | head phone == '0' && length phone == 10 = Nothing
                  | otherwise = Just InvalidPhoneError

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a

  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty

instance Semigroup Int where
  (<>) :: Int -> Int -> Int
  x <> y = x + y

instance Monoid Int where
  mempty :: Int
  mempty = 0

newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where  
  (<>) :: Num a => Sum a -> Sum a -> Sum a
  (Sum x) <> (Sum y) = Sum $ x + y

instance Num a => Monoid (Sum a) where  
  mempty :: Num a => Sum a
  mempty = Sum 0

newtype Product a = Product { getProduct :: a }

instance Num a => Semigroup (Product a) where  
  (<>) :: Num a => Product a -> Product a -> Product a
  (Product x) <> (Product y) = Product $ x * y

instance Num a => Monoid (Product a) where  
  mempty :: Num a => Product a
  mempty = Product 1

doubleSum :: [Double] -> Double
doubleSum l = getSum $ mconcat (map Sum l)

doubleProduct :: [Double] -> Double
doubleProduct l = getProduct $ mconcat (map Product l)

instance Semigroup a => Semigroup (Maybe a) where  
  (<>) :: Semigroup a => Maybe a -> Maybe a -> Maybe a
  Nothing <> Nothing = Nothing
  a@(Just x) <> Nothing = a
  Nothing <> a@(Just x) = a
  (Just x) <> (Just y) = Just $ x <> y

instance Monoid a => Monoid (Maybe a) where  
  mempty :: Monoid a => Maybe a
  mempty = Nothing

combineMaybe :: [Maybe Int] -> Maybe Int
combineMaybe = mconcat 

instance Foldable Chain where  
  foldr :: (a -> b -> b) -> b -> Chain a -> b
  foldr op nv Empty = nv
  foldr op nv (Singleton x) = op x nv
  foldr op nv (Append c1 c2) =
    foldr op (foldr op nv c2) c1

instance Foldable (Either a) where  
  foldr :: (b -> c -> c) -> c -> Either a b -> c
  foldr op nv (Left _) = nv
  foldr op nv (Right x) = op x nv

data Json = 
  JsonNull |
  JsonBool Bool |
  JsonNumber Double |
  JsonString String |
  JsonArray [Json] |
  JsonObject [(String, Json)]

class JsonSerializable a where
  toJson :: a -> Json
  toJsonString :: a -> String
  toJsonString = show . toJson 

joinMap :: (a -> String) -> String -> [a] -> String
joinMap f delim [] = ""
joinMap f delim (x:xs) = foldl (\result current -> result ++ delim ++ f current) (f x) xs

instance Show Json where
  show :: Json -> String
  show JsonNull = "null"
  show (JsonBool b) = 
    let (x:xs) = show b
    in toEnum (fromEnum x - fromEnum 'A' + fromEnum 'a') : xs
  show (JsonNumber d) = show d
  show (JsonString s) = show s
  show (JsonArray l) = "[" ++ joinMap show ", " l ++ "]"
  show (JsonObject l) = "{" ++ joinMap (\(key, value) -> "\"" ++ key ++ "\":" ++ show value) ",\n" l ++ "}"

instance JsonSerializable a => JsonSerializable (Maybe a) where 
  toJson :: JsonSerializable a => Maybe a -> Json
  toJson Nothing = JsonNull
  toJson (Just x) = toJson x

instance JsonSerializable String where
  toJson :: String -> Json
  toJson = JsonString
  
instance JsonSerializable Student where  
  toJson :: Student -> Json
  toJson (Student fn name email phone) = 
    JsonObject [
      ("fn", toJson fn),
      ("name", toJson name),
      ("email", toJson email),
      ("phone", toJson phone)
    ]

student1 :: Student
student1 = Student "3MI0800092" "Georgi Atanasov" "nigosto@gmail.com" Nothing

student2 :: Student
student2 = Student "0MI0800065" "Rosen Kolev" "master_troppical@gmail.com" (Just "0888547128")

instance Semigroup Json where
  (<>) :: Json -> Json -> Json
  (JsonArray l1) <> (JsonArray l2) = JsonArray $ l1 ++ l2
  x <> (JsonArray l) = JsonArray $ x:l
  x <> y = JsonArray [x, y]

instance Monoid Json where 
  mempty :: Json
  mempty = JsonArray []

serializeStudents :: [Student] -> String
serializeStudents = show . mconcat . map toJson 

class Functor f where
  fmap :: (a -> b) -> f a -> f b

  (<$>) :: (a -> b) -> f a -> f b
  (<$>) = fmap

instance Functor Maybe where  
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Functor (Either a) where  
  fmap :: (b -> c) -> Either a b -> Either a c
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right $ f b
