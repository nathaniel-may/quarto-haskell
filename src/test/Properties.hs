{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.List (intercalate)

unsplit :: Char -> [String] -> String
unsplit c = intercalate [c]

split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

prop_split_inv xs
    = forAll (elements xs) $ \c ->
      unsplit c (split c xs) == xs

prop_split_inv2 xs
    = prop_split_inv

return []
main = $quickCheckAll