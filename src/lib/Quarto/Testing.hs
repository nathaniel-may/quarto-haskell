{-# Language LambdaCase #-}

{-|
Module      : Quarto.Testing
Description : Quickcheck Instances

This module provides Arbitrary instances for testing with QuickCheck
-}
module Quarto.Testing where

import qualified Data.Map as Map
import Data.Bifunctor (bimap, first)
import Data.Either (fromRight)
import Control.Exception (Exception, displayException)
import Test.QuickCheck

import qualified Quarto as Q
import Quarto
import Quarto.Types.Internal
import Quarto.Lib


-- * QuickCheck instances

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Turns where
  arbitrary = someTurns
  shrink (Turns []) = []
  shrink (Turns ts) = [Turns (init ts)]

instance Arbitrary Quarto where
  arbitrary = takeTurns <$> arbitrary
  shrink (Final (FinalQuarto b _)) = Pass . passQuarto <$> shrink b
  shrink (Pass  (PassQuarto  b))   = Pass . passQuarto <$> shrink b
  shrink (Place (PlaceQuarto b p)) =
    mapRights (fmap Place . flip placeQuarto p) (shrink b)
      ++ shrink (Pass (passQuarto b))

instance Arbitrary Board where
  arbitrary        = MkBoard . Map.fromList <$> (sublistOf =<< placements)
  shrink (Board b) = [ MkBoard $ Map.deleteAt i b | i <- [0 .. length b - 1] ]

instance Arbitrary HIndex where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary VIndex where
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

-- * Types

data QuartoTestException = QuartoE QuartoException | TestE TestException
                         deriving (Eq, Show, Read)

data TestException = MismatchedTurn
                   deriving (Eq, Show, Read)

instance Exception TestException where
  displayException MismatchedTurn  = "attempted mismatched turn"

data Turn = PassTurn Player Piece | PlaceTurn Player Tile
          deriving (Eq, Show, Read)

newtype Turns = Turns { turns :: [Turn] }
              deriving (Eq, Show, Read)

-- * Functions

allTurns :: Gen Turns
allTurns = Turns . concatMap mkTurns <$> (zip players <$> placements)
    where mkTurns ((pa, pb), (t, p)) = [PassTurn pa p, PlaceTurn pb t]

someTurns :: Gen Turns
someTurns = Turns <$> (take <$> elements [0..16] <*> (turns <$> allTurns))

arbFinalGame :: Gen FinalQuarto
arbFinalGame = toFinal . takeTurns <$> allTurns where
  toFinal = \case
    Final q -> q
    _       -> error "game with all pieces wasn't over" -- unreachable

takeTurn :: Turn -> Quarto -> Either QuartoTestException Quarto
takeTurn _              q@(Final _) = Right q
takeTurn (PassTurn  pl p) (Pass q)  = first QuartoE $ Place <$> pass q pl p
takeTurn (PlaceTurn pl t) (Place q) = first QuartoE $ fromEither . bimap Pass Final <$> Q.place q pl t
takeTurn _ _                        = Left (TestE MismatchedTurn)

takeTurns :: Turns -> Quarto
takeTurns ts = foldl (\q t -> fromRight q $ takeTurn t q) Q.empty (turns ts)

shrinkBoundedEnum :: (Eq a, Enum a, Bounded a) => a -> [a]
shrinkBoundedEnum x = takeWhile (/= x) enumerate

placements :: Gen [(Tile, Piece)]
placements = do
  sTiles  <- shuffle allTiles
  sPieces <- shuffle allPieces
  pure (sTiles `zip` sPieces)

-- infinite list of [(P1, P2), (P2, P1) ...]
players :: [(Player, Player)]
players = cycle [(P1, P2), (P2, P1)]