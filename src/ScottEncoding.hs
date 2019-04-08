-- https://www.codewars.com/kata/59c132fb70a3b7efd3000024

{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

unPair :: (a -> b -> c) -> SPair a b -> c
unPair f q = (runPair q) f

toPair :: SPair a b -> (a, b)
toPair sp = unPair (\a b -> (a, b)) sp

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\p -> p a b)

fst :: SPair a b -> a
fst sp = unPair (\x _ -> x) sp

snd :: SPair a b -> b
snd sp = unPair (\_ y -> y) sp

swap :: SPair a b -> SPair b a
swap sp = fromPair (snd sp, fst sp)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \x y -> f $ fromPair (x, y)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \sp -> f (fst sp) (snd sp)

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }

unMaybe :: b -> (a -> b) -> SMaybe a -> b
unMaybe b f q = (runMaybe q) b f

justM :: a -> SMaybe a
justM a = SMaybe $ \b f -> f a

nothingM :: SMaybe a
nothingM = SMaybe $ \b f -> b

toMaybe :: SMaybe a -> Maybe a
toMaybe s = unMaybe Nothing Just s

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing  = nothingM
fromMaybe (Just x) = justM x

isJust :: SMaybe a -> Bool
isJust = not . isNothing

isNothing :: SMaybe a -> Bool
isNothing s = case (toMaybe s) of
                Nothing -> True
                _       -> False

trans :: Maybe a -> a
trans (Just x) = x

catMaybes :: SList (SMaybe a) -> SList a
catMaybes lm = map (trans . toMaybe) $ flt isJust lm


newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }

unEither :: (a -> c) -> (b -> c) -> SEither a b -> c
unEither f g q = (runEither q) f g

left' :: a -> SEither a b
left' a = SEither $ \f g -> f a

right' :: b -> SEither a b
right' b = SEither $ \f g -> g b

toEither :: SEither a b -> Either a b
toEither se = unEither Left Right se

fromEither :: Either a b -> SEither a b
fromEither (Left a)  = left' a
fromEither (Right b) = right' b

isLeft :: SEither a b -> Bool
isLeft se = case (toEither se) of
              Left _ -> True
              _      -> False

isRight :: SEither a b -> Bool
isRight = not . isLeft

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition le = fromPair (l, r)
  where
    l = map ((\(Left x)  -> x) . toEither) $ flt isLeft  le
    r = map ((\(Right x) -> x) . toEither) $ flt isRight le


newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }

toList :: SList a -> [a]
toList sl = foldr (:) [] sl

fromList :: [a] -> SList a
fromList (x:xs) = cons x (fromList xs)
fromList []     = nil'

nil' :: SList a
nil' = SList const

cons :: a -> SList a -> SList a
cons x xs = SList (\ni co -> co x xs)

concat :: SList a -> SList a -> SList a
concat xs ys = foldr cons ys xs

runList' :: b -> (a -> SList a -> b) -> SList a -> b
runList' ni co (SList f) = f ni co

null :: SList a -> Bool
null = runList' True (\_ _ -> False)

hd :: b -> (a -> b) -> SList a -> b
hd b f q = runList' b (\a _ -> f a) q

tl :: SList a -> SList a
tl q = runList' nil' (\_ q' -> q') q

length :: SList a -> Int
length sl = case (null sl) of
              True -> 0
              _    -> 1 + length (tl sl)

map :: (a -> b) -> SList a -> SList b
map f = runList' nil' (\x xs -> cons (f x) (map f xs))

flt :: (a -> Bool) -> SList a -> SList a
flt f = runList' nil' (\x xs -> if (f x) then cons x (flt f xs) else (flt f xs))

zip :: SList a -> SList b -> SList (SPair a b)
zip sl sp = if null sl || null sp then nil' else (cons (fromPair (hd (error "head") id sl, hd (error "head") id sp)) (zip (tl sl) (tl sp)))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f d xs = (runList xs) d (\h t -> foldl f (f d h) t)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f d xs = (runList xs) d (\h t -> f h (foldr f d t))

take :: Int -> SList a -> SList a
take n _ | n <= 0 = nil'
take n sl
  | null sl   = nil'
  | otherwise = cons (hd (error "head") id sl) (take (n-1) (tl sl))