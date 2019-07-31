module Quarto where

import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

import Board


data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)

-- TODO smart constructors --
newtype PassQuarto  = PassQuarto  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = PlaceQuarto Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = FinalQuarto Board GameEnd deriving (Eq, Show, Read)

data Quarto = Pass PassQuarto | Place PlaceQuarto | Final FinalQuarto

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