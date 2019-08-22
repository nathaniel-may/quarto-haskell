module Quarto.Internal.Lib where

import Data.List (delete)
import Data.Either (rights)


fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

-- mapRights
mapRights :: (a -> Either c b) -> [a] -> [b]
mapRights f xs =
  rights $ map f xs
  -- snd . partitionEithers $ map f xs
  -- [x | Right x <- f <$> xs]
  -- fromEither . mapBoth (const []) (: []) =<< (f <$> xs)

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound..maxBound]

allUnique :: Eq a => [a] -> Bool
allUnique []     = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

-- bimap
mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

-- first (Bifunctor, in Data.Bifunctor)
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id

-- Data.List.intersect
same :: Eq a => [a] -> [a] -> [a]
same [] _ = []
same _ [] = []
same (x:xs) ys = if x `elem` ys
                 then x : same xs (delete x ys)
                 else same xs ys