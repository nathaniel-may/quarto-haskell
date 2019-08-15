{-# LANGUAGE PatternSynonyms #-}

module Quarto.Internal.Types where

import Data.Map (Map, elems)
import Control.Exception (Exception, displayException)

import Quarto.Internal.Lib


data Color  = White | Black  deriving (Eq, Enum, Ord, Bounded, Read)
data Shape  = Round | Square deriving (Eq, Enum, Ord, Bounded, Read)
data Height = Tall  | Short  deriving (Eq, Enum, Ord, Bounded, Read)
data Top    = Flat  | Hole   deriving (Eq, Enum, Ord, Bounded, Read)

instance Show Color where
  show Black = "B"
  show White = "W"

instance Show Shape where
  show Round  = "R"
  show Square = "Q"

instance Show Height where
  show Tall  = "T"
  show Short = "S"

instance Show Top where
  show Flat = "F"
  show Hole = "H"

data Attribute = W | B | R | Q | S | T | F | H
               deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Property = PropColor  Color
              | PropShape  Shape
              | PropHeight Height
              | PropTop    Top

data Piece = Piece Color Shape Height Top
           deriving (Eq, Read)

instance Show Piece where
  show = show <> show <> show <> show

data HIndex = HA | HB | HC | HD
           deriving (Eq, Enum, Ord, Bounded, Read)

data VIndex = V1 | V2 | V3 | V4
          deriving (Eq, Enum, Ord, Bounded, Read)

instance Show HIndex where
  show HA = "A"
  show HB = "B"
  show HC = "C"
  show HD = "D"

instance Show VIndex where
  show V1 = "1"
  show V2 = "2"
  show V3 = "3"
  show V4 = "4"

data Tile = Tile HIndex VIndex deriving (Eq, Ord, Bounded, Show, Read)

-- TODO smart constructor checks --
newtype Board = MkBoardUnsafe { tiles :: Map Tile Piece }
              deriving (Eq, Show, Read)

board :: Map Tile Piece -> Either QuartoException Board
board m | allUnique (elems m)
          = Right (MkBoardUnsafe m)
        | otherwise
          = Left BoardPiecesMustBeUnique

pattern Board :: Map Tile Piece -> Board
pattern Board m <- MkBoardUnsafe m

{-# COMPLETE Board #-}

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

data Line = Horizontal HIndex
          | Vertical VIndex
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
                     | BoardPiecesMustBeUnique
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
  displayException BoardPiecesMustBeUnique    = "to create a board all pieces on the board must be unique"
  displayException PieceAlreadyOnBoard        = "piece is already on the board"
  displayException FinalQuartoMustBeCompleted = "cannot create a FinalQuarto with a board that isn't a win or a tie"
  displayException FinishedGameHasNoTurn      = "game is over. it is no one's turn."
  displayException CannotPassOffTurn          = "cannot pass when it's not your turn."
  displayException CannotPassPlacedPiece      = "cannot pass a piece that is already on the board."
  displayException CannotPlaceOffTurn         = "cannot place when it's not your turn."
  displayException CannotPlaceOnOccupiedTile  = "cannot place on a tile that is already occupied on the board"