{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

import Test.QuickCheck
import Data.List (zip4)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)

import Quarto (Player(P1), Player(P2))
import Board (
    Board(..)
  , Piece(..)
  , Tile(..)
  , Color(..)
  , Shape(..)
  , Height(..)
  , Top(..)
  , indexes
  , place
  , contains
  , containsPiece
  , isFull)
import Lib (uncurry4, third)

allPieces :: [Piece]
allPieces =
  uncurry4 Piece <$>
  zip4 [Black, White] [Round, Square] [Tall, Short] [Flat, Hole]

allTiles :: [Tile]
allTiles = uncurry Tile <$> [(h,v) | h <- indexes, v <- indexes]

playerGen :: Gen Player
playerGen = elements [P1, P2]

boardGen :: Gen Board
boardGen = Board . Map.fromList <$> do
  tiles  <- sublistOf =<< shuffle allTiles
  pieces <- sublistOf =<< shuffle allPieces
  pure $ zip tiles pieces

boardPlayerGen :: Gen (Board, Player)
boardPlayerGen = do
  b <- boardGen
  (,) b <$> playerGen

prop_boardPlace
    = forAll (do
        b <- boardGen
        t <- elements allTiles
        (,,) b t <$> elements allPieces) (\case
          (b, t, p) ->
            if b `contains` t || b `containsPiece` p
            then isNothing $ place b t p
            else place b t p == (Just . Board . Map.insert t p $ tiles b))

prop_boardContains
    = forAll (do
        b <- boardGen
        (,) b <$> elements allTiles) (\case
          (b, t) ->
            if not . null . Map.lookup t $ tiles b
            then b `contains` t
            else not $ b `contains` t)

prop_boardContainsPiece
    = forAll (do
        b <- boardGen
        (,) b <$> elements allPieces) (\case
          (b, p) ->
            if elem p $ tiles b
            then b `containsPiece` p
            else not $ b `containsPiece` p)

prop_boardIsFull
    = forAll boardGen (\b ->
      if 16 == length (tiles b)
      then isFull b
      else not $ isFull b)

return []
main = $quickCheckAll