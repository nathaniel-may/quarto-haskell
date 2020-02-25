{-# LANGUAGE PatternSynonyms #-}

module Quarto.Types.Internal where

import Data.Map (Map, elems)
import Control.Exception (Exception, displayException)

import Quarto.Lib

-- Types and Instances --

data Color  = White | Black  deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Shape  = Round | Square deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Height = Tall  | Short  deriving (Eq, Enum, Ord, Bounded, Show, Read)
data Top    = Flat  | Hole   deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Attribute = W | B | R | Q | S | T | F | H
               deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Property = PropColor  Color
              | PropShape  Shape
              | PropHeight Height
              | PropTop    Top
              deriving (Eq, Ord, Show, Read)

data Piece = Piece Color Shape Height Top
           deriving (Eq, Ord, Show, Read)

data HIndex = HA | HB | HC | HD
           deriving (Eq, Enum, Ord, Bounded, Show, Read)

data VIndex = V1 | V2 | V3 | V4
          deriving (Eq, Enum, Ord, Bounded, Show, Read)

data Tile = Tile HIndex VIndex deriving (Eq, Ord, Bounded, Show, Read)

data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player [WinningLine] | Tie deriving (Eq, Ord, Show, Read)

data Quarto = Pass PassQuarto | Place PlaceQuarto | Final FinalQuarto
            deriving (Eq, Ord, Show, Read)

data Line = Horizontal HIndex
          | Vertical VIndex
          | DiagonalForward
          | DiagonalBackward
          deriving (Eq, Ord, Show, Read)

data WinningLine = WinningLine { wlLine      :: Line
                               , wlAttribute :: Attribute }
                 deriving (Eq, Ord, Show, Read)

-- Private Constructors --

-- game logic assumes these types have only been constructed via their associated
-- smart constructors. Using them directly may cause incorrect behavior.

newtype Board = MkBoard { tiles :: Map Tile Piece }
              deriving (Eq, Ord, Show, Read)

newtype PassQuarto  = MkPassQuarto  Board         deriving (Eq, Ord, Show, Read)
data    PlaceQuarto = MkPlaceQuarto Board Piece   deriving (Eq, Ord, Show, Read)
data    FinalQuarto = MkFinalQuarto Board GameEnd deriving (Eq, Ord, Show, Read)

board :: Map Tile Piece -> Either QuartoException Board
board m | allUnique (elems m)
          = Right (MkBoard m)
        | otherwise
          = Left BoardPiecesMustBeUnique

-- Pattern Matching --

pattern Board :: Map Tile Piece -> Board
pattern Board m <- MkBoard m
{-# COMPLETE Board #-}

pattern PassQuarto :: Board -> PassQuarto
pattern PassQuarto  b   <- MkPassQuarto  b
{-# COMPLETE PassQuarto #-}

pattern PlaceQuarto :: Board -> Piece -> PlaceQuarto
pattern PlaceQuarto b p <- MkPlaceQuarto b p
{-# COMPLETE PlaceQuarto #-}

pattern FinalQuarto :: Board -> GameEnd -> FinalQuarto
pattern FinalQuarto b e <- MkFinalQuarto b e
{-# COMPLETE FinalQuarto #-}

-- Exceptions --

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