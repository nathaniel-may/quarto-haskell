{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Data.List (zip4, unfoldr, inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Either.Combinators (mapBoth)
import Data.Either.Utils (fromEither)
import qualified Prelude as P
import Prelude

import qualified Quarto as Q
import Quarto
import qualified Board as B
import Board hiding (Property)

newtype Turns = Turns [(Player, Tile, Piece)]

-- if and only if --
iff :: Bool -> Bool -> Bool
iff = (==)
infix 3 `iff`

shrinkBoundedEnum :: (Eq a, Enum a, Bounded a) => a -> [a]
shrinkBoundedEnum x = takeWhile (/= x) [minBound..maxBound]

placements :: Gen [(Tile, Piece)]
placements = do
  tiles  <- shuffle allTiles
  pieces <- shuffle allPieces
  pure $ zip tiles pieces

-- infinite list of [P1, P2, P1, P2 ...]
players :: [Player]
players = unfoldr (\pl -> if pl == P1 then Just (P2, P2) else Just (P1, P1)) P1

takeTurn :: (Player, Tile, Piece) -> Quarto -> Maybe Quarto
takeTurn _ (Final _)          = Nothing
takeTurn (pl, _, p) (Pass q)  = Place <$> pass q pl p
takeTurn (pl, t, _) (Place q) = fromEither . mapBoth Pass Final <$> Q.place q pl t


instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Turns where
  arbitrary = Turns . fmap (\(a,(b,c)) -> (a,b,c)) . zip players <$> placements
  shrink (Turns turns) = Turns <$> inits turns

-- TODO --
instance Arbitrary Quarto where
  arbitrary = undefined
  shrink    = undefined

instance Arbitrary Board where
  arbitrary        = Board . Map.fromList <$> (sublistOf =<< placements)
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
    = isNothing $ B.place b t p
  | otherwise
    = B.place b t p == Just (Board . Map.insert t p $ tiles b)

prop_boardContains :: Board -> Tile -> Bool
prop_boardContains b t =
  (not . null . Map.lookup t $ tiles b) `iff` (b `contains` t)

prop_boardContainsPiece :: Board -> Piece -> Bool
prop_boardContainsPiece b p = (p `elem` tiles b) `iff` (b `containsPiece` p)

prop_fullBoard :: Board -> Bool
prop_fullBoard b = length (tiles b) == 16 `iff` full b

prop_turn :: Quarto -> Bool
prop_turn q @ (Final _)  = isNothing $ turn q
prop_turn q @ (Pass qq)  = turn q == if B.even (board q) then Just P1 else Just P2
prop_turn q @ (Place qq) = turn q == if B.even (board q) then Just P2 else Just P1

pure []

main :: IO Bool
main = $quickCheckAll