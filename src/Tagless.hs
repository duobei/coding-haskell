-- https://www.codewars.com/kata/5424e3bc430ca2e577000048

{-# LANGUAGE RankNTypes #-}

module Tagless where

import Prelude hiding (and, or)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)
  
  loop   :: r h (a -> a) -> r h a
  
  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool
  
  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool
  
  ifte   :: r h Bool -> r h a -> r h a -> r h a

newtype R h a = R { run :: h -> a }

unit :: a -> R h a
unit = R . const

one :: (a -> b) -> R h a -> R h b
one op e = R (op . run e)

two :: (a -> b -> c) -> R h a -> R h b -> R h c
two op e1 e2 = R $ \h -> op (run e1 h) (run e2 h)

instance Language R where
    here     = R fst
    before e = R (run e . snd)
    lambda e = R $ \h x -> run e (x, h)
    apply    = two ($)

    loop e = R (fix . run e)

    int  = unit
    add  = two (+)
    down = one pred
    up   = one succ
    mult = two (*)
    gte  = two (>=)

    bool = unit
    and  = two (&&)
    or   = two (||)
    neg  = one not

    ifte e1 e2 e3 = R $ \h -> if run e1 h then run e2 h else run e3 h

type Term a = forall r h . Language r => r h a

fix f = f (fix f)

interpret :: Term a -> a
interpret t = run t ()