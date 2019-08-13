{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import Test.QuickCheck

import Prelude
import Data.List
import Data.Map ()
import qualified Data.Map as Map
import Data.Either
import Data.Foldable
import Control.Exception

import qualified Quarto as Q
import Quarto
import qualified Board as B
import Board hiding (Property)
import Errors

data Turn = PassTurn Player Piece | PlaceTurn Player Tile
          deriving (Eq, Show, Read)

newtype Turns = Turns { turns :: [Turn] }
              deriving (Eq, Show, Read)

data QuartoTestException = QuartoE QuartoException | TestE TestException
                         deriving (Eq, Show, Read)

data TestException = MismatchedTurn | MismatchedTurnA | MismatchedTurnB Quarto Player Tile
                   deriving (Eq, Show, Read)

instance Exception TestException where
  displayException MismatchedTurn  = "attempted mismatched turn"

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

shrinkBoundedEnum :: (Eq a, Enum a, Bounded a) => a -> [a]
shrinkBoundedEnum x = takeWhile (/= x) [minBound..maxBound]

placements :: Gen [(Tile, Piece)]
placements = do
  sTiles  <- shuffle allTiles
  sPieces <- shuffle allPieces
  pure $ zip sTiles sPieces

-- infinite list of [(P1, P2), (P2, P1) ...]
players :: [(Player, Player)]
players = cycle [(P1, P2), (P2, P1)]

takeTurn :: Turn -> Quarto -> Either QuartoTestException Quarto
takeTurn _              q@(Final _) = Right q
takeTurn (PassTurn  pl p) (Pass q)  = mapLeft QuartoE $ Place <$> pass q pl p
takeTurn (PlaceTurn pl t) (Place q) = mapLeft QuartoE $ fromEither . mapBoth Pass Final <$> Q.place q pl t
-- takeTurn _ _                        = Left (TestE MismatchedTurn)
takeTurn (PassTurn _ _) (Place _)   = Left (TestE MismatchedTurnA)
takeTurn (PlaceTurn pl t) (Pass q)  = Left (TestE (MismatchedTurnB (Pass q) pl t))

takeTurns :: Turns -> Quarto
takeTurns ts = foldl (\q t -> fromRight q $ takeTurn t q) Q.empty (turns ts)

takeTurnsWithErrors :: Turns -> Either QuartoTestException Quarto
takeTurnsWithErrors ts = foldlM (flip takeTurn) Q.empty (turns ts)

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Turns where
  arbitrary = Turns . concatMap mkTurns <$> (zip players <$> placements) -- TODO this is where my error is?
    where mkTurns ((pa, pb), (t, p)) = [PassTurn pa p, PlaceTurn pb t]
  shrink (Turns [])    = []
  shrink (Turns turns) = [Turns (init turns)]

instance Arbitrary Quarto where
  arbitrary = takeTurns <$> arbitrary
  shrink (Final (FinalQuarto b _)) = Pass . passQuarto <$> shrink b
  shrink (Pass  (PassQuarto  b))   = Pass . passQuarto <$> shrink b
  shrink (Place (PlaceQuarto b p)) =
    mapEither (fmap Place . flip placeQuarto p) (shrink b)
      ++ shrink (Pass (passQuarto b))

instance Arbitrary Board where
  arbitrary        = Board . Map.fromList <$> (sublistOf =<< placements)
  shrink (Board b) = [Board $ Map.deleteAt i b | i <- [0 .. length b - 1]]

instance Arbitrary Index where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Tile where
  arbitrary = Tile <$> arbitrary <*> arbitrary
  shrink (Tile x y) =  [Tile x' y  | x' <- shrink x]
                    ++ [Tile x  y' | y' <- shrink y]

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Shape where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Height where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Top where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Piece where
  arbitrary = Piece <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Piece c s h t) =
       [Piece c' s  h  t  | c' <- shrink c]
    ++ [Piece c  s' h  t  | s' <- shrink s]
    ++ [Piece c  s  h' t  | h' <- shrink h]
    ++ [Piece c  s  h  t' | t' <- shrink t]

prop_boardPlace :: Board -> Tile -> Piece -> Bool
prop_boardPlace b t p
  | b `contains` t || b `containsPiece` p
    = isLeft $ B.place b t p
  | otherwise
    = B.place b t p == Right (Board . Map.insert t p $ tiles b)

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
prop_turn q@(Pass _)   = turn q == if B.even (board q) then Right P1 else Right P2
prop_turn q@(Place _)  = turn q == if B.even (board q) then Right P2 else Right P1

prop_activePieceNotPlaced :: Tile -> Piece -> Bool
prop_activePieceNotPlaced t p = isLeft $ flip placeQuarto p =<< B.place B.empty t p

prop_p1MustStart :: Player -> Piece -> Bool
prop_p1MustStart pl p = pl == P2 && rejected ||
                        pl == P1 && not rejected
  where rejected = isLeft (pass (passQuarto B.empty) pl p)

prop_meta_turnsNeverRejected :: Turns -> Bool
prop_meta_turnsNeverRejected ts = isRight (takeTurnsWithErrors ts)


pure []

main :: IO Bool
main = $quickCheckAll