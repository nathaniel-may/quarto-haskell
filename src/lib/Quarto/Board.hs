module Quarto.Board where

import qualified Data.Map as Map
import Data.Maybe (Maybe)

import Quarto.Internal.Types
import Quarto.Lib


empty :: Board
empty = MkBoard Map.empty

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

allPieces :: [Piece]
allPieces = [ Piece c s h t | c <- enumerate
                            , s <- enumerate
                            , h <- enumerate
                            , t <- enumerate ]

allTiles :: [Tile]
allTiles = [ Tile h v | h <- enumerate, v <- enumerate ]

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
  | b `contains` t       = Left TileOccupied
    -- this check could be deferred to the board but this exception is clearer for this function
  | b `containsPiece` p  = Left PieceAlreadyPlaced
  | otherwise            = board . Map.insert t p $ tiles b

even :: Board -> Bool
even = Prelude.even . size