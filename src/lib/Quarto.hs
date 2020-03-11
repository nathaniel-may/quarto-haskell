{-# Language LambdaCase #-}

module Quarto (
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
  , final
  , finalState
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


-- * Smart Constructors

{-|
 PassQuarto constructor is not actually smart. This construction is used 
 for consistency across Quarto types.
-}
passQuarto :: Board -> PassQuarto
passQuarto = MkPassQuarto

-- |PlaceQuarto smart constructor. Fails if the piece to place is already on the board
placeQuarto :: Board -> Piece -> Either QuartoException PlaceQuarto
placeQuarto b p
  | B.containsPiece p b
    = Left PieceAlreadyOnBoard
  | otherwise
    = Right (MkPlaceQuarto b p)


-- |FinalQuarto smart constructor. Fails if there is no winning line and the board isn't full
finalQuarto :: Board -> Either QuartoException FinalQuarto
finalQuarto b
  | null winLines && full b
    = Right (MkFinalQuarto b Tie)
  | (not . null) winLines
    = MkFinalQuarto b . flip Winner winLines <$> (turn . Pass $ passQuarto b)
  | otherwise
    = Left FinalQuartoMustBeCompleted
  where winLines = winningLines b

-- * Functions

-- |The initial state of a Quarto game.
empty :: Quarto
empty = Pass $ passQuarto B.empty

-- |Given a game, it is either a player's turn or the game is over.
turn :: Quarto -> Either QuartoException Player
turn (Pass  (PassQuarto  b))   = if even b then Right P1 else Right P2
turn (Place (PlaceQuarto b _)) = if even b then Right P2 else Right P1
turn (Final _)                 = Left FinishedGameHasNoTurn

-- |Determines if it is this player's turn or not
isTurn :: Quarto -> Player -> Bool
isTurn q pl = turn q == Right pl

-- |Returns the board from a Quarto value.
getBoard :: Quarto -> Board
getBoard (Pass  (PassQuarto  b))   = b
getBoard (Place (PlaceQuarto b _)) = b
getBoard (Final (FinalQuarto b _)) = b

-- |Counts the number of pieces placed in a Quarto game.
piecesPlaced :: Quarto -> Int
piecesPlaced = size . getBoard

-- |Gets the piece placed on the given tile from the board of a quarto game.
getPiece :: Tile -> Quarto -> Maybe Piece
getPiece t q = get t (getBoard q)

-- |Gets the passed piece from a quarto game if there is one.
getPassedPiece :: Quarto -> Maybe Piece
getPassedPiece (Place (PlaceQuarto _ p)) = Just p
getPassedPiece _                         = Nothing

-- |Determines if the quarto game has the piece in play (on the board or passed).
containsPiece :: Piece -> Quarto -> Bool
containsPiece p q = B.containsPiece p (getBoard q) || (p `elem` getPassedPiece q)

-- |Lists all pieces not in play.
availablePieces :: Quarto -> [Piece]
availablePieces (Final _) = []
availablePieces q         = [x | x <- allPieces, not (containsPiece x q)]

-- |Lists all pieces in play.
unavailablePieces :: Quarto -> [Piece]
unavailablePieces (Final _) = allPieces
unavailablePieces q         = [x | x <- allPieces, containsPiece x q]

-- |Takes a turn when the game is in a PassQuarto state
pass :: PassQuarto -> Player -> Piece -> Either QuartoException PlaceQuarto
pass q@(PassQuarto b) pl p
  | not $ isTurn (Pass q) pl
    = Left CannotPassOffTurn
  | B.containsPiece p b
    = Left CannotPassPlacedPiece
  | otherwise
    = placeQuarto b p

-- |Takes a turn when the game is in a PlaceQuarto state
place :: PlaceQuarto -> Player -> Tile -> Either QuartoException (Either PassQuarto FinalQuarto)
place q@(PlaceQuarto b p) pl t
  | not $ isTurn (Place q) pl
    = Left CannotPlaceOffTurn
  | B.contains t b
    = Left CannotPlaceOnOccupiedTile
  | otherwise
    = B.place t p b <&> \nb ->
        first (const $ passQuarto nb) (finalQuarto nb)

-- |Determines if the game is in a final state or not
final :: Quarto -> Bool
final (Final _) = True
final _         = False

-- |Given a game state, if it is a finished game it pulls out the end result. 
finalState :: Quarto -> Maybe GameEnd
finalState = \case
    Final (FinalQuarto _ Tie) -> Just Tie
    Final (FinalQuarto _ win) -> Just win
    _ -> Nothing

-- |List of all possible winning lines
lines :: [Line]
lines = [DiagonalForward, DiagonalBackward]
     <> (Horizontal <$> enumerate)
     <> (Vertical   <$> enumerate)

-- |Gets the list of tiles that make up a given line.
lineTiles :: Line -> [Tile]
lineTiles (Vertical   i)   = flip Tile i <$> enumerate
lineTiles (Horizontal i)   =      Tile i <$> enumerate
lineTiles DiagonalForward  = zipWith Tile enumerate (reverse enumerate)
lineTiles DiagonalBackward = zipWith Tile enumerate enumerate

{-|
  For a given line, returns a list of all wins on that line. A single line
  can have multiple wins if for instance all pieces in the line are the same
  color and the same shape.
-}
winsForLine :: Line -> Board -> [WinningLine]
winsForLine line b = fmap (WinningLine line)
                   . concatMap (foldr1 intersect)
                   . nonEmpty
                   . (\x -> if length x == 4 then x else [])
                   . mapMaybe (fmap attrs . flip get b) $ lineTiles line

-- |From a board returns the list of all winning lines
winningLines :: Board -> [WinningLine]
winningLines b = flip winsForLine b =<< lines