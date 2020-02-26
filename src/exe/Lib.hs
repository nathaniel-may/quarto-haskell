module Lib where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple (swap)

invertMap :: (Ord k, Ord v) => Bimap k v -> Bimap v k    
invertMap = BM.fromList . fmap swap . BM.toList

zipR :: (Maybe a, Maybe b) -> Maybe (a, b)
zipR (Just a, Just b) = Just (a, b)
zipR _                = Nothing

firstTwo :: Text -> Maybe (Char, Char)
firstTwo t = zipR (headMay t, headMay (T.drop 1 t))

headMay :: Text -> Maybe Char
headMay t = if T.null t then Nothing else Just (T.head t)

showCharNoQuotes :: Char -> String
showCharNoQuotes = trim . show where
    trim (_ : xs) = init xs
    trim _        = undefined -- unreachable

flatten2x2 :: ((a, b), (c, d)) -> (a, b, c, d)
flatten2x2 ((w, x), (y, z)) = (w, x, y, z)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs) where
  h = length xs `div` 2

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

-- | Case analysis for MaybeT
maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT mb kb (MaybeT ma) = maybe mb kb =<< ma

repeatM :: Monad m => (a -> MaybeT m a) -> a -> m a
repeatM f = g
     where g x = maybeT (pure x) g (f x)