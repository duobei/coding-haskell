-- https://www.codewars.com/kata/5917f22dd2563a36a200009c

module ISO where

import Data.Void

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus (ab, ba) (cd, dc) = (either (Left . ab) (Right . cd), either (Left . ba) (Right . dc))

isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS (ab, ba) = (\x -> h ab x, \y -> h ba y)
                where h fun arg =
                        case arg of
                          Nothing -> Nothing
                          Just x  -> Just (fun x)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)

-- Now, algebraic data type got the name because
-- it satisfy a lot of algebraic rule under isomorphism

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (either Right Left, either Right Left)

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (first, second)
  where
    first = \x -> case x of
        Left (Left a) -> Left a
        Left (Right b) -> Right (Left b)
        Right c -> Right (Right c)
        
    second = \x -> case x of
        Left a -> Left (Left a)
        Right (Left b) -> Left (Right b)
        Right (Right c) -> Right c

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (s, s)
  where s (x, y) = (y, x)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (first, second)
        where
          first  = \((a, b), c) -> (a, (b, c))
          second = \(a, (b, c)) -> ((a, b), c)

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (first, second)
  where
    first (a, Left b) = Left (a, b)
    first (a, Right c) = Right (a, c)
    second (Left (a, b))= (a, Left b)
    second (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (left, right)
  where
    left = \x -> case x of
                  True  -> Nothing
                  False -> Just Nothing
    right = \x -> case x of
                   Nothing -> True
                   Just Nothing -> False
                   Just (Just a) -> absurd a

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- given a void, we can have anything
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (left, right)
  where
    left (Left Nothing)    = Nothing
    left (Left (Just a))   = Just (Left a)
    left (Right b)         = Just (Right b)
    right Nothing          = Left Nothing
    right (Just (Left a))  = Left (Just a)
    right (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (fst, absurd)

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (left, right)
  where
    left (Nothing, b) = Left b
    left (Just a, b)  = Right (a, b)
    right (Left b) = (Nothing, b)
    right (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans` 
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (left, right)
  where
    left  = \mba -> (mba Nothing, mba . Just)
    right = \(a, ba) -> \mb -> case mb of
                                    Nothing -> a
                                    Just b -> ba b

-- a ^ 1 = a
-- Go the hard way to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = (\f -> f (), const) --_ `trans` powS `trans` _
-- Here's a trick: 
-- replace undefined with the rhs of the comment on previous line
-- When you're not sure what to fill in for a value,
-- Have it as a _
-- GHC will goes like
-- "Found hole `_' with type: (Num (ISO (() -> a) (Maybe b0 -> a0)))"
-- So you can immediately see value of what type are needed
-- This process can be repeat indefinitely -
-- For example you might replace `_` with `isoFunc _ _`
-- So GHC hint you on more specific type.
-- This is especially usefull if you have complex type.
-- See https://wiki.haskell.org/GHC/Typed_holes
-- And "stepwise refinement" for more details.