{-|
Module      : Lib
Description : Helper Functions

Functions which are more generally useful, and not part of 
a convenient package.
-}
module Lib where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Text (Text)
import qualified Data.Text as T

-- * String & Text

{-|
  Gets the first two characters in a string. Nothing for strings 
  shorter than two characters.
-}
firstTwo :: Text -> Maybe (Char, Char)
firstTwo t = crossProduct (textHeadMay t) (textHeadMay $ T.drop 1 t)

-- |head without exceptions for 'Foldable's.
headMay :: Foldable f => f a -> Maybe a
headMay = foldr (const . pure) Nothing

-- |head without exceptions for 'Text'.
textHeadMay :: Text -> Maybe Char
textHeadMay t = if T.null t then Nothing else Just (T.head t)

-- |string representation of a character with no quotes.
showCharNoQuotes :: Char -> String
showCharNoQuotes = trim . show where
    trim (_ : c : _) = [c]
    trim _           = "" -- unreachable

-- * Tuples

-- |takes a pair of pairs and makes them a single tuple of 4.
flatten2x2 :: ((a, b), (c, d)) -> (a, b, c, d)
flatten2x2 ((w, x), (y, z)) = (w, x, y, z)

-- |halves a list. If the length is odd, the second half will be longer.
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs) where
  h = length xs `div` 2

{-|
  Cross product for Applicative values.

  Cross product name most apparent with lists:
  @
  > crossProduct [1,2,3] [True, False]
  [(1,True),(1,False),(2,True),(2,False),(3,True),(3,False)]
  @

  Particularly useful with 'Maybe':
  @
  > crossProduct (Just 1) (Just True)
  Just (1, True)
  > crossProduct Nothing (Just True)
  Nothing
  > crossProduct (Just 1) Nothing
  Nothing
  @
-}
crossProduct :: Applicative f => f a -> f b -> f (a, b)
crossProduct = liftA2 (,)

-- * Either

-- |Converts an 'Either' value to a 'Maybe' by erasing the error.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

-- * Bifunctor

-- |maps both sides of a Bifunctor when they are the same type.
bimapBoth :: Bifunctor p => (a -> b) -> p a a -> p b b
bimapBoth = join bimap

-- * Maybe T

-- |Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure