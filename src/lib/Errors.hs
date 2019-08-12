{-# LANGUAGE DeriveAnyClass #-}

module Errors where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Exception


data QuartoException = GameException Text | BoardException Text | TestException Text
                     deriving (Eq, Show, Read, Exception)

-- Board Exceptions --

tileOccupied :: QuartoException
tileOccupied = BoardException $ T.pack
  "cannot place a piece on an already occupied tile"

pieceAlreadyPlaced :: QuartoException
pieceAlreadyPlaced = BoardException $ T.pack
  "cannot place a piece that is already on the board"

-- Game Exceptions --

pieceAlreadyOnBoard :: QuartoException
pieceAlreadyOnBoard = GameException $ T.pack
  "piece is already on the board"

finalQuartoMustBeCompleted :: QuartoException
finalQuartoMustBeCompleted = GameException $ T.pack
  "cannot create a FinalQuarto with a board that isn't a win or a tie"

finishedGameHasNoTurn :: QuartoException
finishedGameHasNoTurn = GameException $ T.pack
  "game is over. it is no one's turn."

cannotPassOffTurn :: QuartoException
cannotPassOffTurn = GameException $ T.pack
  "cannot pass when it's not your turn."

cannotPassPlacedPiece :: QuartoException
cannotPassPlacedPiece = GameException $ T.pack
  "cannot pass a piece that is already on the board."

cannotPlaceOffTurn :: QuartoException
cannotPlaceOffTurn = GameException $ T.pack
  "cannot place when it's not your turn."

cannotPlaceOnOccupiedTile :: QuartoException
cannotPlaceOnOccupiedTile = GameException $ T.pack
  "cannot place on a tile that is already occupied on the board"

-- Exceptions used in internal tests

mismatchedTurn :: QuartoException
mismatchedTurn = TestException $ T.pack
  "attempted mismatched turn"

errrrr :: QuartoException
errrrr = GameException $ T.pack
  ""