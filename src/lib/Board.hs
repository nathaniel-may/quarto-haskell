module Board where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe, catMaybes)
import qualified Data.Maybe as Maybe
import qualified Data.List as List

data Color  = Black | White  deriving (Eq, Ord, Show, Read)
data Shape  = Round | Square deriving (Eq, Ord, Show, Read)
data Height = Tall  | Short  deriving (Eq, Ord, Show, Read)
data Top    = Flat  | Hole   deriving (Eq, Ord, Show, Read)

data Attribute = W | B | R | Q | S | T | F | H
               deriving (Eq, Show, Read)

data Property = PropColor Color
              | PropShape Shape
              | PropHeight Height
              | PropTop Top

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

data Piece = Piece Color Shape Height Top
           deriving (Eq, Ord, Show, Read)

attrs :: Piece -> [Attribute]
attrs (Piece c s h t) = [attr $ PropColor c,
                         attr $ PropShape s,
                         attr $ PropHeight h,
                         attr $ PropTop t]

data Index = I1 | I2 | I3 | I4
           deriving (Eq, Ord, Enum, Bounded, Show, Read)

indexes :: [Index]
indexes = [minBound..maxBound]

data Tile = Tile Index Index deriving (Eq, Ord, Show, Read)

-- TODO smart constructor checks --
newtype Board = Board { tiles :: Map Tile Piece }
              deriving (Eq, Ord, Show, Read)

isFull :: Board -> Bool
isFull b = size b >= 16

-- TODO int between 0 and 16 --
size :: Board -> Int
size = length . tiles

contains :: Board -> Tile -> Bool
contains b = not . null . get b

containsPiece :: Board -> Piece -> Bool
containsPiece b p = elem p $ tiles b

-- TODO can I get rid of the maybe with LH? --
get :: Board -> Tile -> Maybe Piece
get b t = Map.lookup t $ tiles b

-- TODO how many restrictions can I place here with LH? --
place :: Board -> Tile -> Piece -> Maybe Board
place b t p = if not (contains b t) && not (containsPiece b p)
              then Just . Board . Map.insert t p $ tiles b
              else Nothing

isEven :: Board -> Bool
isEven b = size b `mod` 2 == 0

data Line = Horizontal Index
          | Vertical Index
          | DiagonalForward
          | DiagonalBackward
          deriving (Eq, Show, Read)

data WinningLine = WinningLine Line Attribute

lines :: [Line]
lines = [DiagonalForward, DiagonalBackward] <>
        (Horizontal <$> indexes) <>
        (Vertical   <$> indexes)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> indexes
lineTiles (Horizontal i)   =      Tile i <$> indexes
lineTiles DiagonalForward  = uncurry Tile <$> indexes `zip` reverse indexes
lineTiles DiagonalBackward = uncurry Tile <$> reverse indexes `zip` indexes

same :: Eq a => [a] -> [a] -> [a]
same (x:xs) ys = if x `elem` ys
                 then x : same xs (List.delete x ys)
                 else same xs ys
same _ _ = []

isWin :: Board -> Line -> [WinningLine]
-- isWin = undefined
isWin b line =
  fmap (WinningLine line) .
  foldr same [] .
  filter (\x -> 4 == length x) .
  catMaybes $
  fmap attrs .
  get b <$> lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = isWin b =<< Board.lines

test :: [Int] -> [Int]
test xs = (+3) . (+2) <$> xs