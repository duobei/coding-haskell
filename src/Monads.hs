{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return x = Identity x
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where 
  return a        = State $ \s -> (a,s)
  (State x) >>= f = State $ \s -> let (v,s') = x s in runState (f v) s'

instance Monad (Reader s) where
  return a         = Reader $ \e -> a 
  (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e

instance Monoid w => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (s, v)) >>= f = let (s', a') = runWriter $ f v in Writer (s `mappend` s', a')