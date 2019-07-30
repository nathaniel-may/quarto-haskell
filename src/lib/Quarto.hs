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