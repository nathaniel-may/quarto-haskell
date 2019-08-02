module Quarto where

import Data.Maybe (Maybe, catMaybes)
import qualified Data.Maybe as Maybe

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
pass q pl p = case q of
  PassQuarto b -> if isTurn (Pass q) pl && not(containsPiece b p)
                  then Just $ PlaceQuarto b p
                  else Nothing

-- TODO requires is won --
place :: PlaceQuarto -> Player -> Maybe (Either PassQuarto FinalQuarto)
-- place q pl = (==pl) <$> turn (Place q) <&>
place = undefined

lines :: [Line]
lines = [DiagonalForward, DiagonalBackward] <>
        (Horizontal <$> indexes) <>
        (Vertical   <$> indexes)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> indexes
lineTiles (Horizontal i)   =      Tile i <$> indexes
lineTiles DiagonalForward  = uncurry Tile <$> indexes `zip` reverse indexes
lineTiles DiagonalBackward = uncurry Tile <$> reverse indexes `zip` indexes

isWin :: Board -> Line -> [WinningLine]
isWin b line =
  fmap (WinningLine line) .
  foldr same [] .
  filter (\x -> 4 == length x) .
  catMaybes $
  fmap attrs .
  get b <$> lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = isWin b =<< Quarto.lines