{-# LANGUAGE PatternSynonyms #-}

module Quarto.Types where

import Data.Map (Map)
import Control.Exception (Exception, displayException)


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


pattern PassQuarto :: Board -> PassQuarto
pattern PassQuarto  b   <- MkPassQuartoUnsafe  b

pattern PlaceQuarto :: Board -> Piece -> PlaceQuarto
pattern PlaceQuarto b p <- MkPlaceQuartoUnsafe b p

pattern FinalQuarto :: Board -> GameEnd -> FinalQuarto
pattern FinalQuarto b e <- MkFinalQuartoUnsafe b e

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

-- game logic assumes these types have only been constructed via their associated
-- smart constructors. Using them directly may cause incorrect behavior.
newtype PassQuarto  = MkPassQuartoUnsafe  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = MkPlaceQuartoUnsafe Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = MkFinalQuartoUnsafe Board GameEnd deriving (Eq, Show, Read)

-- Exceptions

data QuartoException = TileOccupied
                     | PieceAlreadyPlaced
                     | PieceAlreadyOnBoard
                     | FinalQuartoMustBeCompleted
                     | FinishedGameHasNoTurn
                     | CannotPassOffTurn
                     | CannotPassPlacedPiece
                     | CannotPlaceOffTurn
                     | CannotPlaceOnOccupiedTile
                     deriving (Eq, Show, Read)

instance Exception QuartoException where
  displayException TileOccupied               = "cannot place a piece on an already occupied tile"
  displayException PieceAlreadyPlaced         = "cannot place a piece that is already on the board"
  displayException PieceAlreadyOnBoard        = "piece is already on the board"
  displayException FinalQuartoMustBeCompleted = "cannot create a FinalQuarto with a board that isn't a win or a tie"
  displayException FinishedGameHasNoTurn      = "game is over. it is no one's turn."
  displayException CannotPassOffTurn          = "cannot pass when it's not your turn."
  displayException CannotPassPlacedPiece      = "cannot pass a piece that is already on the board."
  displayException CannotPlaceOffTurn         = "cannot place when it's not your turn."
  displayException CannotPlaceOnOccupiedTile  = "cannot place on a tile that is already occupied on the board"