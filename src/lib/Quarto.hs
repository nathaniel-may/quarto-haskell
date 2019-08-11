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
  , winningLines
  -- * functions that should be in a library
  , fromEither
  ) where

import Prelude hiding (lines, even)

import Data.Maybe
import Data.List (delete)
import Data.List.NonEmpty (nonEmpty)
import Control.Applicative hiding (empty)

import qualified Board as B
import Board hiding (empty, place)
import Errors

data Player = P1 | P2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

data GameEnd = Winner Player | Tie deriving (Eq, Show, Read)

newtype PassQuarto  = MkPassQuarto  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = MkPlaceQuarto Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = MkFinalQuarto Board GameEnd deriving (Eq, Show, Read)

-- not smart. used for consistency across Quarto types
passQuarto :: Board -> PassQuarto
passQuarto = MkPassQuarto

placeQuarto :: Board -> Piece -> Either Err PlaceQuarto
placeQuarto b p
  | b `containsPiece` p
    = Left (err "cannot create a PlaceQuarto with a piece that is already on the board")
  | otherwise
    = Right (MkPlaceQuarto b p)

finalQuarto :: Board -> Either Err FinalQuarto
finalQuarto b
  | not win && full b
    = Right (MkFinalQuarto b Tie)
  | win
    = MkFinalQuarto b . Winner <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left (err "cannot create a FinalQuarto with a board that isn't a win or a tie")
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

turn :: Quarto -> Either Err Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left (err "game is over. it is no one's turn.")

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

board :: Quarto -> Board
board (Pass (PassQuarto  b))    = b
board (Place (PlaceQuarto b _)) = b
board (Final (FinalQuarto b _)) = b

pass :: PassQuarto -> Player -> Piece -> Either Err PlaceQuarto
pass q@(PassQuarto b) pl p
  | not $ isTurn (Pass q) pl
    = Left (err "cannot pass when it's not your turn.")
  | b `containsPiece` p
    = Left (err "cannot pass a piece that is already on the board.")
  | otherwise
    = placeQuarto b p

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

place :: PlaceQuarto -> Player -> Tile -> Either Err (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t
  | not $ isTurn (Place q) pl
    = Left (err "cannot place when it's not your turn.")
  | b `contains` t
    = Left (err "cannot place on a tile that is already occupied on the board")
  | otherwise
    = maybe (Left (err "")) Right $ rightToMaybe (Left . passQuarto <$> newBoard)
        <|> rightToMaybe (fmap Right . finalQuarto =<< newBoard)
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