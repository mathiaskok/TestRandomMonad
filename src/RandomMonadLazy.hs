module RandomMonadLazy(module RandomMonadLazy) where

import Control.Monad.Trans.State.Lazy
import System.Random

import RandomMonad

liftRandStateT :: (RandomGen g, Monad m) => (Int -> a) -> StateT g m a
liftRandStateT f = StateT $ \g ->
  let (i, nextG) = next g in
  return (f i, nextG)

randValueStateT :: (Random a, RandomGen g, Monad m) => StateT g m a
randValueStateT = StateT $ \g -> return $ random g


instance (RandomGen g, Monad m) => RandomMonad (StateT g m) where
  liftRand = liftRandStateT
  randValue = randValueStateT
