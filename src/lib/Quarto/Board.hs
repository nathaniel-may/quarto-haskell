module Quarto.Board where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)

import Quarto.Errors


data Color  = Black | White  deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Shape  = Round | Square deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Height = Tall  | Short  deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Top    = Flat  | Hole   deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Attribute = W | B | R | Q | S | T | F | H
               deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Property = PropColor  Color
              | PropShape  Shape
              | PropHeight Height
              | PropTop    Top

data Piece = Piece Color Shape Height Top
           deriving (Eq, Show, Read)

data Index = I1 | I2 | I3 | I4
           deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Tile = Tile Index Index deriving (Eq, Ord, Bounded, Show, Read)

-- TODO smart constructor checks --
newtype Board = Board { tiles :: Map Tile Piece }
              deriving (Eq, Show, Read)


empty :: Board
empty = Board Map.empty

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

allPieces :: [Piece]
allPieces = [ Piece c s h t | c <- [minBound..maxBound]
                            , s <- [minBound..maxBound]
                            , h <- [minBound..maxBound]
                            , t <- [minBound..maxBound] ]

allTiles :: [Tile]
allTiles = [ Tile h v | h <- indexes, v <- indexes ]

full :: Board -> Bool
full b = size b >= 16

-- TODO int between 0 and 16 --
size :: Board -> Int
size = length . tiles

contains :: Board -> Tile -> Bool
contains b = not . null . get b

containsPiece :: Board -> Piece -> Bool
containsPiece b p = p `elem` tiles b

-- TODO can I get rid of the maybe with LH? --
get :: Board -> Tile -> Maybe Piece
get b t = Map.lookup t $ tiles b

-- TODO how many restrictions can I place here with LH? --
place :: Board -> Tile -> Piece -> Either QuartoException Board
place b t p
  | b `contains` t
    = Left TileOccupied
  | b `containsPiece` p
    = Left PieceAlreadyPlaced
  | otherwise
    = Right . Board . Map.insert t p $ tiles b

-- TODO drop is
even :: Board -> Bool
even = Prelude.even . size