module Quarto (
  -- TODO make this export list include all that my users need
  -- * constructors
    Quarto(..)
  , Player(..)
  , GameEnd(..)
  , Line(..)
  , WinningLine(..)
  , Piece(..)
  , Color(..)
  , Shape(..)
  , Height(..)
  , Top(..)
  , Tile(..)
  , HIndex(..)
  , VIndex(..)
  -- * smart constructors
  , PassQuarto(PassQuarto),   passQuarto
  , PlaceQuarto(PlaceQuarto), placeQuarto
  , FinalQuarto(FinalQuarto), finalQuarto
  -- * functions
  , allPieces
  , allTiles
  , availablePieces
  , containsPiece
  , empty
  , getPassedPiece
  , getPiece
  , turn
  , isTurn
  , getBoard
  , pass
  , place
  , piecesPlaced
  , lines
  , lineTiles
  , unavailablePieces
  , winsForLine
  , winningLines) where

import Prelude hiding (lines, even)
import Data.Maybe
import Data.List (intersect)
import Data.List.NonEmpty (nonEmpty)
import Data.Functor
import Data.Bifunctor

import Quarto.Types.Internal
import qualified Quarto.Board as B
import Quarto.Board hiding (empty, place, containsPiece)
import Quarto.Lib


-- Smart Constructors --

-- not smart. used for consistency across Quarto types
passQuarto :: Board -> PassQuarto
passQuarto = MkPassQuarto

placeQuarto :: Board -> Piece -> Either QuartoException PlaceQuarto
placeQuarto b p
  | b `B.containsPiece` p
    = Left PieceAlreadyOnBoard
  | otherwise
    = Right (MkPlaceQuarto b p)

finalQuarto :: Board -> Either QuartoException FinalQuarto
finalQuarto b
  | null winLines && full b
    = Right (MkFinalQuarto b Tie)
  | (not . null) winLines
    = MkFinalQuarto b . flip Winner winLines <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left FinalQuartoMustBeCompleted
  where winLines = winningLines b

-- Functions --

empty :: Quarto
empty = Pass $ passQuarto B.empty

turn :: Quarto -> Either QuartoException Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left FinishedGameHasNoTurn

isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

getBoard :: Quarto -> Board
getBoard (Pass ( PassQuarto  b))   = b
getBoard (Place (PlaceQuarto b _)) = b
getBoard (Final (FinalQuarto b _)) = b

piecesPlaced :: Quarto -> Int
piecesPlaced = size . getBoard

getPiece :: Tile -> Quarto -> Maybe Piece
getPiece t q = get (getBoard q) t

getPassedPiece :: Quarto -> Maybe Piece
getPassedPiece (Place (PlaceQuarto _ p)) = Just p
getPassedPiece _                         = Nothing

containsPiece :: Piece -> Quarto -> Bool
containsPiece p q = B.containsPiece (getBoard q) p || (p `elem` getPassedPiece q)

availablePieces :: Quarto -> [Piece]
availablePieces (Final _) = []
availablePieces q         = [x | x <- allPieces, not (containsPiece x q)]

unavailablePieces :: Quarto -> [Piece]
unavailablePieces (Final _) = allPieces
unavailablePieces q         = [x | x <- allPieces, containsPiece x q]

pass :: PassQuarto -> Player -> Piece -> Either QuartoException PlaceQuarto
pass q@(PassQuarto b) pl p
  | not $ isTurn (Pass q) pl
    = Left CannotPassOffTurn
  | b `B.containsPiece` p
    = Left CannotPassPlacedPiece
  | otherwise
    = placeQuarto b p

place :: PlaceQuarto -> Player -> Tile -> Either QuartoException (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t
  | not $ isTurn (Place q) pl
    = Left CannotPlaceOffTurn
  | b `contains` t
    = Left CannotPlaceOnOccupiedTile
  | otherwise
    = B.place b t p <&> \nb ->
        first (const $ passQuarto nb) (finalQuarto nb)

lines :: [Line]
lines = [DiagonalForward, DiagonalBackward]
     <> (Horizontal <$> enumerate)
     <> (Vertical   <$> enumerate)

-- TODO list of size 4 --
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> enumerate
lineTiles (Horizontal i)   =      Tile i <$> enumerate
lineTiles DiagonalForward  = zipWith Tile enumerate (reverse enumerate)
lineTiles DiagonalBackward = zipWith Tile enumerate enumerate

winsForLine :: Board -> Line -> [WinningLine]
winsForLine b line = fmap (WinningLine line)
                   . concatMap (foldr1 intersect)
                   . nonEmpty
                   . (\x -> if length x == 4 then x else [])
                   . mapMaybe (fmap attrs . get b) $ lineTiles line

winningLines :: Board -> [WinningLine]
winningLines b = winsForLine b =<< lines