{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import Test.QuickCheck

import Data.List
import Data.Map ()
import qualified Data.Map as Map
import Data.Maybe
import Prelude

import qualified Quarto as Q
import Quarto
import qualified Board as B
import Board hiding (Property)

data Turn = PassTurn Player Piece | PlaceTurn Player Tile
          deriving (Eq, Show, Read)

newtype Turns = Turns { turns :: [Turn] }
              deriving (Eq, Show, Read)

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

takeTurn :: Turn -> Quarto -> Maybe Quarto
takeTurn _              q@(Final _) = Just q
takeTurn (PassTurn  pl p) (Pass q)  = Place <$> pass q pl p
takeTurn (PlaceTurn pl t) (Place q) = fromEither . mapBoth Pass Final <$> Q.place q pl t
takeTurn _ _                        = Nothing

takeTurns :: Turns -> Quarto
takeTurns ts = foldr (\t q -> fromMaybe q $ takeTurn t q) Q.empty (turns ts)

takeTurnsWithErrors :: Turns -> Maybe Quarto
takeTurnsWithErrors ts = foldr (\t q -> takeTurn t =<< q) (Just Q.empty) (turns ts)

fromEither :: Either a a -> a
fromEither (Left a)  = a
fromEither (Right a) = a

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum
  shrink    = shrinkBoundedEnum

instance Arbitrary Turns where
  arbitrary = Turns . concatMap mkTurns <$> (zip players <$> placements)
    where mkTurns ((pa, pb), (t, p)) = [PassTurn pa p, PlaceTurn pb t]
  shrink (Turns [])    = []
  shrink (Turns turns) = [Turns (init turns)]

instance Arbitrary Quarto where
  arbitrary = takeTurns <$> arbitrary
  shrink (Final (FinalQuarto b _)) = Pass . passQuarto <$> shrink b
  shrink (Pass  (PassQuarto  b))   = Pass . passQuarto <$> shrink b
  shrink (Place (PlaceQuarto b p)) =
    mapMaybe (fmap Place . flip placeQuarto p) (shrink b)
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
    = isNothing $ B.place b t p
  | otherwise
    = B.place b t p == Just (Board . Map.insert t p $ tiles b)

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
prop_turn q@(Final _)  = isNothing $ turn q
prop_turn q@(Pass _)   = turn q == if B.even (board q) then Just P1 else Just P2
prop_turn q@(Place _)  = turn q == if B.even (board q) then Just P2 else Just P1

prop_activePieceNotPlaced :: Tile -> Piece -> Bool
prop_activePieceNotPlaced t p = isNothing $ flip placeQuarto p =<< B.place B.empty t p

prop_p1MustStart :: Player -> Piece -> Bool
prop_p1MustStart pl p = pl == P2 && rejected ||
                        pl == P1 && not rejected
  where rejected = isNothing (pass (passQuarto B.empty) pl p)

prop_meta_turnsNeverRejected :: Turns -> Bool
prop_meta_turnsNeverRejected ts = isJust (takeTurnsWithErrors ts)


pure []

main :: IO Bool
main = $quickCheckAll