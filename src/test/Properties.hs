import Test.QuickCheck
import Data.List (intercalate)

unsplit :: Char -> [String] -> String
unsplit c = intercalate [c]

-- show
split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

prop_split_inv xs
    = forAll (elements xs) $ \c ->
      unsplit c (split c xs) == xs

main = quickCheck prop_split_inv
-- /show