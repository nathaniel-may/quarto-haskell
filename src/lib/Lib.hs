module Lib (same) where

import Data.List (delete)

same :: Eq a => [a] -> [a] -> [a]
same (x:xs) ys = if x `elem` ys
                 then x : same xs (delete x ys)
                 else same xs ys
same _ _ = []