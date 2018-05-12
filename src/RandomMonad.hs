module RandomMonad(module RandomMonad) where

import System.Random

class (Monad m) => RandomMonad m where
  liftRand :: (Int -> a) -> m a
  randValue :: (Random a) => m a

randInfinite :: (Random a, RandomMonad m) => m [a]
randInfinite = do
  x <- randValue
  xs <- randInfinite
  return $ x : xs

randInfiniteFrom :: (RandomMonad m) => (Int -> a) -> m [a]
randInfiniteFrom f = do
  x <- liftRand f
  xs <- randInfiniteFrom f
  return $ x : xs