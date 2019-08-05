module Quarto where

import Data.Maybe (Maybe, mapMaybe)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Functor (($>))
import Control.Monad (guard)
import Prelude hiding (lines)

import Board
import Lib (same)


data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)

-- TODO smart constructors --
newtype PassQuarto  = PassQuarto  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = PlaceQuarto Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = FinalQuarto Board GameEnd deriving (Eq, Show, Read)

data Quarto = Pass PassQuarto | Place PlaceQuarto | Final FinalQuarto

data Line = Horizontal Index
          | Vertical Index
          | DiagonalForward
          | DiagonalBackward
          deriving (Eq, Show, Read)

data WinningLine = WinningLine Line Attribute


turn :: Quarto -> Maybe Player
turn q = case q of
  Pass  (PassQuarto  b  ) -> if isEven b then Just P1 else Just P2
  Place (PlaceQuarto b _) -> if isEven b then Just P2 else Just P1
  Final _                 -> Nothing

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Just pl

pass :: PassQuarto -> Player -> Piece -> Maybe PlaceQuarto
pass q @ (PassQuarto b) pl p =
  guard (isTurn (Pass q) pl && not (containsPiece b p)) $> PlaceQuarto b p

place :: PlaceQuarto -> Player -> Tile -> Maybe (Either PassQuarto FinalQuarto)
place q @ (PlaceQuarto b p) pl t = (\newBoard ->
    if null $ winningLines newBoard
    then Left  $ PassQuarto  newBoard
    else Right $ FinalQuarto newBoard (Winner pl))
  <$> (guard (isTurn (Place q) pl)
  *> Board.place b t p)

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

isWin :: Board -> Line -> [WinningLine]
isWin b line = fmap (WinningLine line)
             . concatMap (foldr1 same)
             . nonEmpty
             . filter ((==4) . length)
             . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = isWin b =<< lines