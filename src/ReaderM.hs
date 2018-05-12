module ReaderM(module ReaderM) where

newtype ReaderM r a = ReaderM {runReaderM :: r -> a }

instance Functor (ReaderM r) where
  fmap mapper reader = ReaderM $ \state -> mapper $ runReaderM reader $ state

readerMApply :: ReaderM r (a -> b) -> ReaderM r a -> ReaderM r b
readerMApply (ReaderM ff) (ReaderM fa) =
  ReaderM $ \state -> (ff state) $ (fa state)

instance Applicative (ReaderM r) where
  pure a = ReaderM $ \_ -> a
  (<*>) = readerMApply
  
readerMBind :: ReaderM r a -> (a -> ReaderM r b) -> ReaderM r b
readerMBind (ReaderM fa) binder =
  ReaderM $ \state -> 
    let a = fa state in
    let (ReaderM fb) = binder a in
    fb state

instance Monad (ReaderM r) where
  return = pure
  (>>=) = readerMBind