module Board where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.List as List

import Lib (same)


data Color  = Black | White  deriving (Eq, Show, Read)
data Shape  = Round | Square deriving (Eq, Show, Read)
data Height = Tall  | Short  deriving (Eq, Show, Read)
data Top    = Flat  | Hole   deriving (Eq, Show, Read)

data Attribute = W | B | R | Q | S | T | F | H
               deriving (Eq, Show, Read)

data Property = PropColor Color
              | PropShape Shape
              | PropHeight Height
              | PropTop Top

data Piece = Piece Color Shape Height Top
           deriving (Eq, Show, Read)

data Index = I1 | I2 | I3 | I4
           deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Tile = Tile Index Index deriving (Eq, Ord, Show, Read)

-- TODO smart constructor checks --
newtype Board = Board { tiles :: Map Tile Piece }
              deriving (Eq, Show, Read)


attr :: Property -> Attribute
attr p = case p of
  PropColor  White  -> W
  PropColor  Black  -> B
  PropShape  Round  -> R
  PropShape  Square -> Q
  PropHeight Tall   -> T
  PropHeight Short  -> S
  PropTop    Flat   -> F
  PropTop    Hole   -> H

attrs :: Piece -> [Attribute]
attrs (Piece c s h t) = [attr $ PropColor c,
                         attr $ PropShape s,
                         attr $ PropHeight h,
                         attr $ PropTop t]

indexes :: [Index]
indexes = [minBound..maxBound]

isFull :: Board -> Bool
isFull b = size b >= 16

-- TODO int between 0 and 16 --
size :: Board -> Int
size = length . tiles

contains :: Board -> Tile -> Bool
contains b = not . null . get b

containsPiece :: Board -> Piece -> Bool
containsPiece b p = elem p $ tiles b

-- TODO can I get rid of the maybe with LH? --
get :: Board -> Tile -> Maybe Piece
get b t = Map.lookup t $ tiles b

-- TODO how many restrictions can I place here with LH? --
place :: Board -> Tile -> Piece -> Maybe Board
place b t p = if not (contains b t) && not (b `containsPiece` p)
              then Just . Board . Map.insert t p $ tiles b
              else Nothing

isEven :: Board -> Bool
isEven b = size b `mod` 2 == 0