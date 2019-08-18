{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import Test.QuickCheck

import Prelude
import qualified Data.Map as Map
import Data.Either
import Data.Foldable
import System.Exit

import qualified Quarto.Game as Q
import Quarto.Game
import Quarto.Testing
import Quarto.Internal.Types hiding (Property)
import Quarto.Internal.Lib
import qualified Quarto.Internal.Board as B
import Quarto.Internal.Board

-- Functions --

-- if and only if --
iff :: Bool -> Bool -> Bool
iff = (==)
infix 3 `iff`

-- https://stackoverflow.com/questions/42764847/is-there-a-there-exists-quantifier-in-quickcheck
exists :: Gen a -> (a -> Bool) -> Property
exists gen prop = property (exists' 1000 gen prop)

exists' :: Int -> Gen a -> (a -> Bool) -> Gen Bool
exists' 0 _ _ = return False
exists' n gen prop = do
  a <- gen
  if prop a
    then return True
    else exists' (n - 1) gen prop

takeTurnsWithErrors :: Turns -> Either QuartoTestException Quarto
takeTurnsWithErrors ts = foldlM (flip takeTurn) Q.empty (turns ts)

finalExists :: (FinalQuarto -> Bool) -> Property
finalExists f =
  exists arbitrary (\case
    Final q -> f q
    _       -> False)

recognizesWin :: Line -> Property
recognizesWin line =
  exists arbitrary (\case
    Final (FinalQuarto _ (Winner _ winLines)) -> line `elem` ((\(WinningLine l _) -> l) <$> winLines)
    _ -> False)

-- Properties --

prop_boardPlace :: Board -> Tile -> Piece -> Bool
prop_boardPlace b t p
  | b `contains` t || b `containsPiece` p
    = isLeft $ B.place b t p
  | otherwise
    = B.place b t p == (board . Map.insert t p $ tiles b)

prop_boardContains :: Board -> Tile -> Bool
prop_boardContains b t =
  (not . null . Map.lookup t $ tiles b) `iff` (b `contains` t)

prop_boardContainsPiece :: Board -> Piece -> Bool
prop_boardContainsPiece b p = (p `elem` tiles b) `iff` (b `containsPiece` p)

prop_fullBoard :: Board -> Bool
prop_fullBoard b = length (tiles b) == 16 `iff` full b

prop_meta_FinalQuartoExists :: Property
prop_meta_FinalQuartoExists = exists arbitrary (\case
                                                  Final _ -> True
                                                  _       -> False)

prop_turn :: Quarto -> Bool
prop_turn q@(Final _)  = isLeft $ turn q
prop_turn q@(Pass _)   = turn q == if B.even (getBoard q) then Right P1 else Right P2
prop_turn q@(Place _)  = turn q == if B.even (getBoard q) then Right P2 else Right P1

prop_activePieceNotPlaced :: Tile -> Piece -> Bool
prop_activePieceNotPlaced t p = isLeft $ flip placeQuarto p =<< B.place B.empty t p

prop_p1MustStart :: Player -> Piece -> Bool
prop_p1MustStart pl p = pl == P2 && rejected ||
                        pl == P1 && not rejected
  where rejected = isLeft (pass (passQuarto B.empty) pl p)

prop_meta_turnsNeverRejected :: Turns -> Bool
prop_meta_turnsNeverRejected ts = isRight (takeTurnsWithErrors ts)

prop_finalGamesAlwaysHaveAtLeast4Pieces :: Quarto -> Bool
prop_finalGamesAlwaysHaveAtLeast4Pieces (Final (FinalQuarto b _)) = size b >= 4
prop_finalGamesAlwaysHaveAtLeast4Pieces _                         = True

prop_recognizesTie :: Property
prop_recognizesTie = exists arbitrary (\case
                                          Final (FinalQuarto _ Tie) -> True
                                          _ -> False)

prop_recognizesAllWinLines :: Property
prop_recognizesAllWinLines = conjoin $ recognizesWin
                          <$> DiagonalBackward
                          : DiagonalForward
                          : (Horizontal <$> enumerate)
                          <> (Vertical <$> enumerate)

prop_recognizesMultiWin :: Property
prop_recognizesMultiWin = exists arbitrary (\case
    Final (FinalQuarto _ (Winner _ winLines)) -> length ((\(WinningLine l _) -> l) <$> winLines) > 2
    _ -> False)


pure []

main :: IO Bool
main = $quickCheckAll >>= \case
  True  -> exitSuccess
  False -> exitFailure