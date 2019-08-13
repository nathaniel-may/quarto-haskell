module Errors where

import Control.Exception

-- TODO make these work together in the main code
data QuartoException = GameE GameException | BoardE BoardException | TestE TestException
                     deriving (Eq, Show, Read)

data GameException = PieceAlreadyOnBoard
                   | FinalQuartoMustBeCompleted
                   | FinishedGameHasNoTurn
                   | CannotPassOffTurn
                   | CannotPassPlacedPiece
                   | CannotPlaceOffTurn
                   | CannotPlaceOnOccupiedTile
                   deriving (Eq, Show, Read)

data BoardException = TileOccupied | PieceAlreadyPlaced
                    deriving (Eq, Show, Read)

data TestException = MismatchedTurn
                   deriving (Eq, Show, Read)

instance Exception BoardException where
  displayException TileOccupied       = "cannot place a piece on an already occupied tile"
  displayException PieceAlreadyPlaced = "cannot place a piece that is already on the board"

instance Exception GameException where
  displayException PieceAlreadyOnBoard        = "piece is already on the board"
  displayException FinalQuartoMustBeCompleted = "cannot create a FinalQuarto with a board that isn't a win or a tie"
  displayException FinishedGameHasNoTurn      = "game is over. it is no one's turn."
  displayException CannotPassOffTurn          = "cannot pass when it's not your turn."
  displayException CannotPassPlacedPiece      = "cannot pass a piece that is already on the board."
  displayException CannotPlaceOffTurn         = "cannot place when it's not your turn."
  displayException CannotPlaceOnOccupiedTile  = "cannot place on a tile that is already occupied on the board"

instance Exception TestException where
  displayException MismatchedTurn = "attempted mismatched turn"