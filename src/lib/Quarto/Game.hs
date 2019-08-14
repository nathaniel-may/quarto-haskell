{-# LANGUAGE PatternSynonyms #-}

module Quarto.Game (
  -- * constructors
    Quarto(..)
  , Player(..)
  , GameEnd(..)
  , Line(..)
  , WinningLine(..)
  -- * smart constructors
  , PassQuarto(PassQuarto),   passQuarto
  , PlaceQuarto(PlaceQuarto), placeQuarto
  , FinalQuarto(FinalQuarto), finalQuarto
  -- * functions
  , empty
  , turn
  , isTurn
  , board
  , pass
  , place
  , lines
  , lineTiles
  , winsForLine
  , winningLines) where

import Prelude hiding (lines, even)

import Data.Maybe
import Data.List.NonEmpty (nonEmpty)
import Data.Functor
import Data.Bifunctor

import qualified Quarto.Board as B
import Quarto.Board hiding (empty, place)
import Quarto.Errors
import Quarto.Types
import Quarto.Internal.Lib
import Quarto.Internal.Unsafe


-- not smart. used for consistency across Quarto types
passQuarto :: Board -> PassQuarto
passQuarto = MkPassQuarto

placeQuarto :: Board -> Piece -> Either QuartoException PlaceQuarto
placeQuarto b p
  | b `containsPiece` p
    = Left PieceAlreadyOnBoard
  | otherwise
    = Right (MkPlaceQuarto b p)

finalQuarto :: Board -> Either QuartoException FinalQuarto
finalQuarto b
  | not win && full b
    = Right (MkFinalQuarto b Tie)
  | win
    = MkFinalQuarto b . Winner <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left FinalQuartoMustBeCompleted
  where win = not . null $ winningLines b

pattern PassQuarto :: Board -> PassQuarto
pattern PassQuarto  b   <- MkPassQuarto  b

pattern PlaceQuarto :: Board -> Piece -> PlaceQuarto
pattern PlaceQuarto b p <- MkPlaceQuarto b p

pattern FinalQuarto :: Board -> GameEnd -> FinalQuarto
pattern FinalQuarto b e <- MkFinalQuarto b e

{-# COMPLETE PassQuarto #-}
{-# COMPLETE PlaceQuarto #-}
{-# COMPLETE FinalQuarto #-}

data Quarto = Pass PassQuarto | Place PlaceQuarto | Final FinalQuarto
            deriving (Eq, Show, Read)

data Line = Horizontal Index
          | Vertical Index
          | DiagonalForward
          | DiagonalBackward
          deriving (Eq, Show, Read)

data WinningLine = WinningLine Line Attribute
                 deriving (Eq, Show, Read)

empty :: Quarto
empty = Pass $ passQuarto B.empty

turn :: Quarto -> Either QuartoException Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left FinishedGameHasNoTurn

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

board :: Quarto -> Board
board (Pass (PassQuarto  b))    = b
board (Place (PlaceQuarto b _)) = b
board (Final (FinalQuarto b _)) = b

pass :: PassQuarto -> Player -> Piece -> Either QuartoException PlaceQuarto
pass q@(PassQuarto b) pl p
  | not $ isTurn (Pass q) pl
    = Left CannotPassOffTurn
  | b `containsPiece` p
    = Left CannotPassPlacedPiece
  | otherwise
    = placeQuarto b p

place :: PlaceQuarto -> Player -> Tile -> Either QuartoException (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t
  | not $ isTurn (Place q) pl
    = Left CannotPlaceOffTurn
  | b `contains` t
    = Left CannotPlaceOnOccupiedTile
  | otherwise
    = B.place b t p <&> \nb ->
        first (const $ passQuarto nb) (finalQuarto nb)

lines :: [Line]
lines = [DiagonalForward, DiagonalBackward]
     <> (Horizontal <$> indexes)
     <> (Vertical   <$> indexes)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> indexes
lineTiles (Horizontal i)   =      Tile i <$> indexes
lineTiles DiagonalForward  = zipWith Tile indexes $ reverse indexes
lineTiles DiagonalBackward = zipWith Tile (reverse indexes) indexes

winsForLine :: Board -> Line -> [WinningLine]
winsForLine b line = fmap (WinningLine line)
                   . concatMap (foldr1 same)
                   . nonEmpty
                   . (\x -> if length x == 4 then x else [])
                   . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = winsForLine b =<< lines