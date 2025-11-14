import Prelude hiding (Functor, fmap, (<$>), Applicative, (<*>), liftA2, (*>))

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

instance Applicative Maybe where  
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  _ <*> Nothing = Nothing
  Nothing <*> _ = Nothing
  (Just f) <*> (Just x) = Just $ f x

  pure :: a -> Maybe a
  pure = Just
