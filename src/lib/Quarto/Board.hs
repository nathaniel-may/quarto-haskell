module Quarto.Board where

import qualified Data.Map as Map
import Data.Maybe (Maybe)

import Quarto.Types.Internal
import Quarto.Lib

-- |An empty board.
empty :: Board
empty = MkBoard Map.empty

-- |Isomorphism from Property to Attribute
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

-- |Extracts the attributes from a piece
attrs :: Piece -> [Attribute]
attrs (Piece c s h t) = [attr (PropColor c),
                         attr (PropShape s),
                         attr (PropHeight h),
                         attr (PropTop t)]

-- |List of all possible pieces
allPieces :: [Piece]
allPieces = [ Piece c s h t | c <- enumerate
                            , s <- enumerate
                            , h <- enumerate
                            , t <- enumerate ]

-- |List of all possible tiles
allTiles :: [Tile]
allTiles = [ Tile h v | h <- enumerate, v <- enumerate ]

-- |Determins if a given board has any empty tiles or not
full :: Board -> Bool
full b = size b >= 16

-- |The size of the board is the number of pieces placed on it
size :: Board -> Int
size = length . tiles

-- |Determines if a 'Tile' is occupied on the given 'Board'
contains :: Tile -> Board -> Bool
contains t = not . null . get t

-- |Determines if a 'Piece' is occupied on the given 'Board'
containsPiece :: Piece -> Board -> Bool
containsPiece p b = p `elem` tiles b

-- |Looks up the piece that is placed on the tile if there is one
get :: Tile -> Board -> Maybe Piece
get t b = Map.lookup t (tiles b)

-- |Places a 'Piece' on the specified 'Tile' on the 'Board'
place :: Tile -> Piece -> Board -> Either QuartoException Board
place t p b
  | contains t b       = Left TileOccupied
    -- this check could be deferred to the board but this exception is clearer for this function
  | containsPiece p b  = Left PieceAlreadyPlaced
  | otherwise          = board $ Map.insert t p (tiles b)

-- |Determines if the size of the board is even or not
even :: Board -> Bool
even = Prelude.even . size