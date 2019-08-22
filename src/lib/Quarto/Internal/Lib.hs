module Quarto.Internal.Lib where

import Data.Either (rights)


fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

mapRights :: (a -> Either c b) -> [a] -> [b]
mapRights f xs = rights $ map f xs

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound..maxBound]

allUnique :: Eq a => [a] -> Bool
allUnique []     = True
allUnique (x:xs) = x `notElem` xs && allUnique xs