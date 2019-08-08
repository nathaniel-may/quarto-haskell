{-# LANGUAGE TemplateHaskell, LambdaCase #-}

import Test.QuickCheck

import Data.List (zip4, unfoldr, inits)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Prelude as P
import Prelude

import qualified Quarto as Q
import Quarto
import qualified Board as B
import Board hiding (Property)

newtype Turns = Turns { turns :: [(Player, Tile, Piece)] }

-- if and only if --
iff :: Bool -> Bool -> Bool
iff = (==)
infix 3 `iff`

-- https://stackoverflow.com/questions/42764847/is-there-a-there-exists-quantifier-in-quickcheck
exists :: Gen a -> (a -> Bool) -> Property
exists gen prop = property (exists' 100 gen prop)

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
  tiles  <- shuffle allTiles
  pieces <- shuffle allPieces
  pure $ zip tiles pieces

-- infinite list of [P1, P2, P1, P2 ...]
players :: [Player]
players = unfoldr (\pl -> if pl == P1 then Just (P2, P2) else Just (P1, P1)) P1

takeTurn :: (Player, Tile, Piece) -> Quarto -> Maybe Quarto
takeTurn _ (Final _)          = Nothing
takeTurn (pl, _, p) (Pass q)  = Place <$> pass q pl p
takeTurn (pl, t, _) (Place q) = fromEither . mapBoth Pass Final <$> Q.place q pl t

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
  arbitrary = Turns . fmap (\(a,(b,c)) -> (a,b,c)) . zip players <$> placements
  shrink (Turns turns) = Turns <$> inits turns

instance Arbitrary Quarto where
  arbitrary = foldr (\t q -> fromMaybe q $ takeTurn t q) Q.empty . turns <$> arbitrary
  shrink (Final (FinalQuarto b _)) = Pass . PassQuarto <$> shrink b
  shrink (Pass  (PassQuarto  b))   = Pass . PassQuarto  <$> shrink b
  shrink (Place (PlaceQuarto b p)) = Place . flip PlaceQuarto p <$> shrink b

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
prop_turn q@(Pass qq)  = turn q == if B.even (board q) then Just P1 else Just P2
prop_turn q@(Place qq) = turn q == if B.even (board q) then Just P2 else Just P1

pure []

main :: IO Bool
main = $quickCheckAll