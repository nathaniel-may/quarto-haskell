module Quarto.Board where

import qualified Data.Map as Map
import Data.Maybe (Maybe)

import Quarto.Types.Internal
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

contains :: Tile -> Board -> Bool
contains t = not . null . get t

containsPiece :: Piece -> Board -> Bool
containsPiece p b = p `elem` tiles b

get :: Tile -> Board -> Maybe Piece
get t b = Map.lookup t (tiles b)

place :: Tile -> Piece -> Board -> Either QuartoException Board
place t p b
  | contains t b       = Left TileOccupied
    -- this check could be deferred to the board but this exception is clearer for this function
  | containsPiece p b  = Left PieceAlreadyPlaced
  | otherwise          = board $ Map.insert t p (tiles b)

even :: Board -> Bool
even = Prelude.even . size