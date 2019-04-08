-- https://www.codewars.com/kata/543d218022e0f307fb000173

module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid
import Control.Applicative

newtype Minimum a = Minimum { getMinimum :: Maybe a }
                  deriving (Show, Eq)

instance Ord a => Monoid (Minimum a) where
  mempty = Minimum Nothing
  Minimum a `mappend` Minimum b = Minimum ((min <$> a <*> b) <|> a <|> b)

build g = g (:) []

myToList :: Foldable t => t a -> [a]
myToList t = build (\c n -> myFoldr c n t)

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMinimum . foldMap (Minimum . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z t = appEndo (foldMap (Endo . f) t) z