module Errors where

import Control.Exception


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

-- TODO make private --
data QuartoTestException = QuartoE QuartoException | TestE TestException

data TestException = MismatchedTurn
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

instance Exception TestException where
  displayException MismatchedTurn = "attempted mismatched turn"