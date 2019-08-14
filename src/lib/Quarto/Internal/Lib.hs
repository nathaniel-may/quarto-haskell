module Quarto.Internal.Lib where

import Data.List (delete)
import Data.Either (rights)


fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

mapEither :: (a -> Either c b) -> [a] -> [b]
mapEither f xs =
  rights $ map f xs
  -- snd . partitionEithers $ map f xs
  -- [x | Right x <- f <$> xs]
  -- fromEither . mapBoth (const []) (: []) =<< (f <$> xs)

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
