{-# LANGUAGE PatternSynonyms #-}

module Quarto (
  -- * constructors
    Quarto(..)
  , Player(..)
  , GameEnd(..)
  , Line(..)
  , WinningLine(..)
  -- * smart constructors
  , PassQuarto(PassQuarto),   passQuarto
  , PlaceQuarto(PlaceQuarto), placeQuarto
  , FinalQuarto(FinalQuarto), finalQuarto
  -- * functions
  , empty
  , turn
  , isTurn
  , board
  , pass
  , place
  , lines
  , lineTiles
  , isWin
  , winningLines) where

import Prelude hiding (lines, even)

import Data.Maybe
import Data.List (delete)
import Data.List.NonEmpty (nonEmpty)
import Data.Functor
import Control.Applicative hiding (empty)
import Control.Monad

import qualified Board as B
import Board hiding (empty, place)


data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)

newtype PassQuarto  = MkPassQuarto  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = MkPlaceQuarto Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = MkFinalQuarto Board GameEnd deriving (Eq, Show, Read)

-- not smart. used for consistency across Quarto types
passQuarto :: Board -> PassQuarto
passQuarto = MkPassQuarto

placeQuarto :: Board -> Piece -> Maybe PlaceQuarto
placeQuarto b p = guard (not (b `containsPiece` p)) $> MkPlaceQuarto b p

finalQuarto :: Board -> Maybe FinalQuarto
finalQuarto b
  | not win && full b
    = Just (MkFinalQuarto b Tie)
  | win
    = MkFinalQuarto b . Winner <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Nothing
  where win = not . null $ winningLines b

pattern PassQuarto :: Board -> PassQuarto
pattern PassQuarto  b   <- MkPassQuarto  b

pattern PlaceQuarto :: Board -> Piece -> PlaceQuarto
pattern PlaceQuarto b p <- MkPlaceQuarto b p

pattern FinalQuarto :: Board -> GameEnd -> FinalQuarto
pattern FinalQuarto b e <- MkFinalQuarto b e

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

empty :: Quarto
empty = Pass $ passQuarto B.empty

turn :: Quarto -> Maybe Player
turn (Pass  (PassQuarto  b))   = if even b then Just P1 else Just P2
turn (Place (PlaceQuarto b _)) = if even b then Just P2 else Just P1
turn (Final _)                 = Nothing

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Just pl

board :: Quarto -> Board
board (Pass (PassQuarto  b))    = b
board (Place (PlaceQuarto b _)) = b
board (Final (FinalQuarto b _)) = b

pass :: PassQuarto -> Player -> Piece -> Maybe PlaceQuarto
pass q@(PassQuarto b) pl p =
  guard (isTurn (Pass q) pl && not (containsPiece b p)) *> placeQuarto b p

place :: PlaceQuarto -> Player -> Tile -> Maybe (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t =
  guard (isTurn (Place q) pl) *>
  (Left . passQuarto <$> newBoard) <|> (fmap Right . finalQuarto =<< newBoard)
  where newBoard = B.place b t p

lines :: [Line]
lines = [DiagonalForward, DiagonalBackward]
     <> (Horizontal <$> indexes)
     <> (Vertical   <$> indexes)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> indexes
lineTiles (Horizontal i)   =      Tile i <$> indexes
lineTiles DiagonalForward  = zipWith Tile indexes $ reverse indexes
lineTiles DiagonalBackward = zipWith Tile (reverse indexes) indexes

same :: Eq a => [a] -> [a] -> [a]
same [] _ = []
same _ [] = []
same (x:xs) ys = if x `elem` ys
                 then x : same xs (delete x ys)
                 else same xs ys

isWin :: Board -> Line -> [WinningLine]
isWin b line = fmap (WinningLine line)
             . concatMap (foldr1 same)
             . nonEmpty
             . filter ((==4) . length)
             . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = isWin b =<< lines