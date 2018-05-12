module StateM(module StateM) where

import System.Random

import RandomMonad

newtype StateM s a = StateM {runStateM :: s -> (a, s)}

instance Functor (StateM s) where
  fmap mapper (StateM sa) = StateM $ \state -> 
    let (a, s) = sa state in
    (mapper a, s)

stateMApply :: StateM s (a -> b) -> StateM s a -> StateM s b
stateMApply (StateM sf) (StateM sa) = StateM $ \s ->
  let (f, s') = sf s in
  let (a, s'') = sa s' in
  (f a, s'')

instance Applicative (StateM s) where
  pure a = StateM $ \s -> (a, s)
  (<*>) = stateMApply

stateMBind :: (StateM s a) -> (a -> StateM s b) -> StateM s b
stateMBind (StateM sa) binder = StateM $ \s ->
  let (a, s') = sa s in
  let (StateM sb) = binder a in
  sb s'

instance Monad (StateM s) where
  return = pure
  (>>=) = stateMBind


liftRandStateM :: (RandomGen g) => (Int -> a) -> StateM g a
liftRandStateM f = StateM $ \g ->
  let (i, nextG) = next g in
  (f i, nextG)

randValueStateM :: (Random a, RandomGen g) => StateM g a
randValueStateM = StateM $ random

instance (RandomGen g) => RandomMonad (StateM g) where
  liftRand = liftRandStateM
  randValue = randValueStateM