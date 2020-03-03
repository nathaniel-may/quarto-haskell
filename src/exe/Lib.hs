module Lib where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

-- String & Text --

firstTwo :: Text -> Maybe (Char, Char)
firstTwo t = zipR (headMay t, headMay (T.drop 1 t))

headMay :: Text -> Maybe Char
headMay t = if T.null t then Nothing else Just (T.head t)

showCharNoQuotes :: Char -> String
showCharNoQuotes = trim . show where
    trim (_ : xs) = init xs
    trim _        = undefined -- unreachable

-- Tuples --

flatten2x2 :: ((a, b), (c, d)) -> (a, b, c, d)
flatten2x2 ((w, x), (y, z)) = (w, x, y, z)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs) where
  h = length xs `div` 2

zipR :: (Maybe a, Maybe b) -> Maybe (a, b)
zipR (Just a, Just b) = Just (a, b)
zipR _                = Nothing

-- Either --

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

-- Maybe T --

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return