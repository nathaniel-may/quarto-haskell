module Quarto.Game (
  -- TODO make this export list include all that my users need
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
  , getBoard
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

import Quarto.Internal.Types
import qualified Quarto.Internal.Board as B
import Quarto.Internal.Board hiding (empty, place)
import Quarto.Internal.Lib


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
  | null winLines && full b
    = Right (MkFinalQuarto b Tie)
  | (not . null) winLines
    = MkFinalQuarto b . flip Winner winLines <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left FinalQuartoMustBeCompleted
  where winLines = winningLines b

empty :: Quarto
empty = Pass $ passQuarto B.empty

turn :: Quarto -> Either QuartoException Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left FinishedGameHasNoTurn

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

getBoard :: Quarto -> Board
getBoard (Pass (PassQuarto  b))    = b
getBoard (Place (PlaceQuarto b _)) = b
getBoard (Final (FinalQuarto b _)) = b

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
     <> (Horizontal <$> enumerate)
     <> (Vertical   <$> enumerate)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> enumerate
lineTiles (Horizontal i)   =      Tile i <$> enumerate
lineTiles DiagonalForward  = zipWith Tile enumerate $ reverse enumerate
lineTiles DiagonalBackward = zipWith Tile (reverse enumerate) enumerate

winsForLine :: Board -> Line -> [WinningLine]
winsForLine b line = fmap (WinningLine line)
                   . concatMap (foldr1 same)
                   . nonEmpty
                   . (\x -> if length x == 4 then x else [])
                   . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = winsForLine b =<< lines