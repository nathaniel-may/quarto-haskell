{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.List (zip4)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)

import Quarto (Player(..))
import Board hiding (Property)

-- if and only if --
iff :: Bool -> Bool -> Bool
iff = (==)
infix 3 `iff`

shrinkBoundedEnum :: (Eq a, Enum a, Bounded a) => a -> [a]
shrinkBoundedEnum x = takeWhile (/= x) [minBound..maxBound]

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Board where
  arbitrary = Board . Map.fromList <$> do
    tiles  <- sublistOf =<< shuffle allTiles
    pieces <- sublistOf =<< shuffle allPieces
    pure $ zip tiles pieces
  shrink (Board b) = [Board $ Map.deleteAt i b | i <- [0 .. length b - 1]]

instance Arbitrary Index where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Tile where
  arbitrary = Tile <$> arbitrary <*> arbitrary
  shrink (Tile x y) =  [Tile x' y  | x' <- shrink x]
                    ++ [Tile x  y' | y' <- shrink y]

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Shape where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Height where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Top where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Piece where
  arbitrary = Piece <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Piece c s h t) =
       [Piece c' s  h  t  | c' <- shrink c]
    ++ [Piece c  s' h  t  | s' <- shrink s]
    ++ [Piece c  s  h' t  | h' <- shrink h]
    ++ [Piece c  s  h  t' | t' <- shrink t]

prop_boardPlace :: Board -> Tile -> Piece -> Bool
prop_boardPlace b t p
  | b `contains` t || b `containsPiece` p
    = isNothing $ place b t p
  | otherwise
    = place b t p == Just (Board . Map.insert t p $ tiles b)

prop_boardContains :: Board -> Tile -> Bool
prop_boardContains b t =
  (not . null . Map.lookup t $ tiles b) `iff` (b `contains` t)

prop_boardContainsPiece :: Board -> Piece -> Bool
prop_boardContainsPiece b p = (p `elem` tiles b) `iff` (b `containsPiece` p)

prop_boardIsFull :: Board -> Bool
prop_boardIsFull b = length (tiles b) == 16 `iff` isFull b


pure []

main :: IO Bool
main = $quickCheckAll