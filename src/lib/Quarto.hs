module Quarto where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe

data Color  = Black | White  deriving (Eq, Ord, Show, Read)
data Shape  = Round | Square deriving (Eq, Ord, Show, Read)
data Height = Tall  | Short  deriving (Eq, Ord, Show, Read)
data Top    = Flat  | Hole   deriving (Eq, Ord, Show, Read)

data Piece = Piece Color Shape Height Top deriving (Eq, Ord, Show, Read)

data Index = I1 | I2 | I3 | I4 deriving (Eq, Ord, Enum, Bounded, Show, Read)

indexes :: [Index]
indexes = [minBound..maxBound]

data Tile = Tile Index Index deriving (Eq, Ord, Show, Read)

-- TODO smart constructor checks --
newtype Board = Board { tiles :: Map Tile Piece }
              deriving (Eq, Ord, Show, Read)

isFull :: Board -> Bool
isFull b = size b >= 16

-- TODO int between 0 and 16 --
size :: Board -> Int
size = length . tiles

contains :: Board -> Tile -> Bool
contains b = not . null . get b

-- TODO can I get rid of the maybe with LH? --
get :: Board -> Tile -> Maybe Piece
get b t = Map.lookup t $ tiles b

-- TODO how many restrictions can I place here with LH? --
place :: Board -> Tile -> Piece -> Maybe Board
place b t p = if not (contains b t) && notElem p (tiles b)
              then Just . Board . Map.insert t p $ tiles b
              else Nothing

data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)

-- TODO smart constructors --
data PassQuarto  = PassQuarto  Board Piece   deriving (Eq, Show, Read)
data PlaceQuarto = PlaceQuarto Board Piece   deriving (Eq, Show, Read)
data FinalQuarto = FinalQuarto Board GameEnd deriving (Eq, Show, Read)

isEven :: Board -> Bool
isEven b = size b `mod` 2 == 0

passTurn :: PassQuarto -> Player
passTurn (PassQuarto b _) = if isEven b then P1 else P2

placeTurn :: PlaceQuarto -> Player
placeTurn (PlaceQuarto b _) = if isEven b then P1 else P2