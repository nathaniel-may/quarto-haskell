module Quarto.Lib where

import Data.Either (rights)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)


fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

mapRights :: (a -> Either c b) -> [a] -> [b]
mapRights f xs = rights $ fmap f xs

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound..maxBound]

allUnique :: (Eq a, Ord a) => [a] -> Bool
allUnique = allUnique' Map.empty

allUnique' :: (Eq a, Ord a) => Map a Bool -> [a] -> Bool
allUnique' _ []     = True
allUnique' m (x:xs) = fromMaybe (allUnique' (Map.insert x False m) xs) (Map.lookup x m)