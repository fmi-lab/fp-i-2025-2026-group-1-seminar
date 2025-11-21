import Prelude hiding (Functor, fmap, (<$>), Applicative, (<*>), liftA2, (*>), pure, Monad, return, (>>=), (>>))

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

class Functor f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b
  pure :: a -> f a
  
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 g fa fb = g <$> fa <*> fb

  (*>) :: f a -> f b -> f b
  fa *> fb = liftA2 const fb fa

instance Applicative Maybe where  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  _ <*> Nothing = Nothing
  Nothing <*> _ = Nothing
  (Just f) <*> (Just x) = Just $ f x

  pure :: a -> Maybe a
  pure = Just

data Chain a = Empty | Singleton a | Append (Chain a) (Chain a) deriving Show

instance Functor Chain where
  fmap :: (a -> b) -> Chain a -> Chain b
  fmap _ Empty = Empty
  fmap f (Singleton x) = Singleton $ f x
  fmap f (Append c1 c2) = Append (fmap f c1) (fmap f c2)

instance Applicative Chain where  
  (<*>) :: Chain (a -> b) -> Chain a -> Chain b
  Empty <*> _ = Empty
  (Singleton f) <*> c = f <$> c
  (Append f1 f2) <*> c = Append (f1 <*> c) (f2 <*> c)
  
  pure :: a -> Chain a
  pure = Singleton

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

  return :: a -> m a
  return = pure

  (>>) :: m a -> m b -> m b
  (>>) = (*>)

instance Monad Maybe where  
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a / b

composeMaybe :: Double -> Maybe Double
composeMaybe d = do
  let x = 100
  y <- safeDiv x 5
  z <- safeDiv y d
  safeDiv z 8

-- composeList :: [(Int, Int)]
-- composeList = do
--   x <- [1..5]
--   y <- [1..5]
--   return (x, y)

instance Monad Chain where  
  (>>=) :: Chain a -> (a -> Chain b) -> Chain b
  Empty >>= _ = Empty
  (Singleton x) >>= f = f x
  (Append c1 c2) >>= f = Append (c1 >>= f) (c2 >>= f)

-- composeChain :: Chain Int
-- composeChain = do
--   x <- Append (Singleton 1) (Singleton 2)
--   y <- Append (Singleton 10) (Singleton 20)
--   return (x + y)

