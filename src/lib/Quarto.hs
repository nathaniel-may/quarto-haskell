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
  , mapEither
  , mapBoth
  , mapLeft
  , same
  ) where

import Prelude hiding (lines, even)

import Data.Maybe
import Data.List (delete)
import Data.List.NonEmpty (nonEmpty)
import Data.Either
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

placeQuarto :: Board -> Piece -> Either QuartoException PlaceQuarto
placeQuarto b p
  | b `containsPiece` p
    = Left pieceAlreadyOnBoard
  | otherwise
    = Right (MkPlaceQuarto b p)

finalQuarto :: Board -> Either QuartoException FinalQuarto
finalQuarto b
  | not win && full b
    = Right (MkFinalQuarto b Tie)
  | win
    = MkFinalQuarto b . Winner <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left finalQuartoMustBeCompleted
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

-- Internal Library Functions --

fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

mapEither :: (a -> Either c b) -> [a] -> [b]
mapEither f as = fromEither . mapBoth (const []) (: []) =<< (f <$> as)

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id

same :: Eq a => [a] -> [a] -> [a]
same [] _ = []
same _ [] = []
same (x:xs) ys = if x `elem` ys
                 then x : same xs (delete x ys)
                 else same xs ys

-- Quarto functions

empty :: Quarto
empty = Pass $ passQuarto B.empty

turn :: Quarto -> Either QuartoException Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left finishedGameHasNoTurn

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

board :: Quarto -> Board
board (Pass (PassQuarto  b))    = b
board (Place (PlaceQuarto b _)) = b
board (Final (FinalQuarto b _)) = b

pass :: PassQuarto -> Player -> Piece -> Either QuartoException PlaceQuarto
pass q@(PassQuarto b) pl p
  | not $ isTurn (Pass q) pl
    = Left cannotPassOffTurn
  | b `containsPiece` p
    = Left cannotPassPlacedPiece
  | otherwise
    = placeQuarto b p

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

place :: PlaceQuarto -> Player -> Tile -> Either QuartoException (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t
  | not $ isTurn (Place q) pl
    = Left cannotPlaceOffTurn
  | b `contains` t
    = Left cannotPlaceOnOccupiedTile
  | otherwise
    = (\nb -> fromRight (Left (passQuarto nb)) (Right <$> finalQuarto nb))
      <$> B.place b t p

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

isWin :: Board -> Line -> [WinningLine]
isWin b line = fmap (WinningLine line)
             . concatMap (foldr1 same)
             . nonEmpty
             . filter ((==4) . length)
             . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = isWin b =<< lines