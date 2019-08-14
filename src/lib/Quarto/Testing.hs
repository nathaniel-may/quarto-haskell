module Quarto.Testing where

import qualified Data.Map as Map
import Data.Either (fromRight)
import Control.Exception (Exception, displayException)
import Test.QuickCheck

import Quarto.Types
import qualified Quarto.Game as Q
import Quarto.Game
import Quarto.Board
import Quarto.Internal.Lib


data QuartoTestException = QuartoE QuartoException | TestE TestException
                         deriving (Eq, Show, Read)

data TestException = MismatchedTurn
                   deriving (Eq, Show, Read)

instance Exception TestException where
  displayException MismatchedTurn  = "attempted mismatched turn"

takeTurn :: Turn -> Quarto -> Either QuartoTestException Quarto
takeTurn _              q@(Final _) = Right q
takeTurn (PassTurn  pl p) (Pass q)  = mapLeft QuartoE $ Place <$> pass q pl p
takeTurn (PlaceTurn pl t) (Place q) = mapLeft QuartoE $ fromEither . mapBoth Pass Final <$> Q.place q pl t
takeTurn _ _                        = Left (TestE MismatchedTurn)

takeTurns :: Turns -> Quarto
takeTurns ts = foldl (\q t -> fromRight q $ takeTurn t q) Q.empty (turns ts)

data Turn = PassTurn Player Piece | PlaceTurn Player Tile
          deriving (Eq, Show, Read)

newtype Turns = Turns { turns :: [Turn] }
              deriving (Eq, Show, Read)

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

-- QuickCheck instances

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Turns where
  arbitrary = Turns . concatMap mkTurns <$> (zip players <$> placements)
    where mkTurns ((pa, pb), (t, p)) = [PassTurn pa p, PlaceTurn pb t]
  shrink (Turns [])    = []
  shrink (Turns ts) = [Turns (init ts)]

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