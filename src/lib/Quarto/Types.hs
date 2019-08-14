module Quarto.Types where

import Data.Map (Map)


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

data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)