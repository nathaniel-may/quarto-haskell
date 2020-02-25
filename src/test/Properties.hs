{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import Test.QuickCheck

import Prelude
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Either
import Data.Foldable
import System.Exit

import qualified Quarto as Q
import Quarto
import Quarto.Testing
import Quarto.Types.Internal hiding (Property)
import Quarto.Lib
import qualified Quarto.Board as B
import Quarto.Board

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
finalExists = exists arbFinalGame

recognizesWin :: Line -> Property
recognizesWin line = finalExists $ \case
  FinalQuarto _ (Winner _ winLines) -> line `elem` ((\(WinningLine l _) -> l) <$> winLines)
  FinalQuarto _ Tie -> False

allUniqueSlow :: Eq a => [a] -> Bool
allUniqueSlow []     = True
allUniqueSlow (x:xs) = x `notElem` xs && allUniqueSlow xs

-- Properties --

prop_allUniqueWorks :: (Ord a) => [a] -> Bool
prop_allUniqueWorks as = allUnique as == allUniqueSlow as

prop_boardPlace :: Board -> Tile -> Piece -> Bool
prop_boardPlace b t p
  | b `B.contains` t || b `B.containsPiece` p
    = isLeft $ B.place b t p
  | otherwise
    = B.place b t p == (board . Map.insert t p $ tiles b)

prop_boardContains :: Board -> Tile -> Bool
prop_boardContains b t =
  (not . null . Map.lookup t $ tiles b) `iff` (b `contains` t)

prop_boardContainsPiece :: Board -> Piece -> Bool
prop_boardContainsPiece b p = (p `elem` tiles b) `iff` (b `B.containsPiece` p)

prop_fullBoard :: Board -> Bool
prop_fullBoard b = length (tiles b) == 16 `iff` full b

prop_meta_FinalQuartoExists :: Property
prop_meta_FinalQuartoExists = exists arbitrary (\case
                                                  Final _ -> True
                                                  _       -> False)

prop_turn :: Quarto -> Bool
prop_turn q@(Final _) = isLeft $ turn q
prop_turn q@(Pass _)  = turn q == if B.even (getBoard q) then Right P1 else Right P2
prop_turn q@(Place _) = turn q == if B.even (getBoard q) then Right P2 else Right P1

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
prop_recognizesTie = finalExists (\case
                                    (FinalQuarto _ Tie) -> True
                                    _ -> False)

prop_recognizesAllWinLines :: Property
prop_recognizesAllWinLines = conjoin $ recognizesWin <$> 
  [DiagonalBackward, DiagonalForward] <> (Horizontal <$> enumerate) <> (Vertical <$> enumerate)

prop_recognizesMultiLineWin :: Property
prop_recognizesMultiLineWin = finalExists (\case
    (FinalQuarto _ (Winner _ winLines)) -> length ((\(WinningLine l _) -> l) <$> winLines) > 2
    _ -> False)

prop_recognizesMultiAttributeWin :: Property
prop_recognizesMultiAttributeWin = finalExists (\case
    (FinalQuarto _ Tie) -> False
    (FinalQuarto _ (Winner _ winLines)) ->
      any (>=2) . Map.elems $
        foldl
          (\m (WinningLine _ a) -> Map.insert a (fromMaybe (0 :: Int) (Map.lookup a m) + 1) m)
          Map.empty
          winLines)

prop_unavailablePieces :: Quarto -> Bool
prop_unavailablePieces q@(Final _) = 
  allPieces == unavailablePieces q
prop_unavailablePieces q@(Pass (PassQuarto b)) = 
  B.size b == length (unavailablePieces q)
prop_unavailablePieces q@(Place (PlaceQuarto b _)) = 
  (1 + B.size b) == length (unavailablePieces q)

prop_availablePieces :: Quarto -> Bool
prop_availablePieces q@(Final _) = 
  null (availablePieces q)
prop_availablePieces q@(Pass (PassQuarto b)) = 
  (16 - B.size b) == length (availablePieces q)
prop_availablePieces q@(Place (PlaceQuarto b _)) = 
  (15 - B.size b) == length (availablePieces q)

pure []

main :: IO Bool
main = $quickCheckAll >>= \case
  True  -> exitSuccess
  False -> exitFailure