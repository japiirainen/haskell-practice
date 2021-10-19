module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State x) = State $ \s -> let (a, s') = x s in (f a, s')

instance Applicative (State s) where
  State f <*> State g = State $ \s ->
    let (v1, s1) = f s
        (v2, s2) = g s1
     in (v1 v2, s2)

  pure x = State $ \s -> (x, s)

instance Monad (State s) where
  (State x) >>= f = State $ \s ->
    let (a, newState) = x s
        (State g) = f a
     in g newState

  return = pure
