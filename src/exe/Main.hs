{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase #-}

module Main (main) where

import Quarto
import qualified Quarto as Q

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Console.Byline hiding (Menu)

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do

  let game = Q.empty
  sayLn "::::  Let's Play Quarto!  ::::\n"

  let player = case Q.turn game of Left _  -> undefined --game is over
                                   Right p -> undefined

  sayLn (style game)
  sayLn (style pieceMenu)

  let question = "pass a piece: "
  input <- ask question Nothing

  let choice = flip M.lookup pieceMenu =<< headMay (T.toUpper input)
  sayLn $ maybe ("not a valid piece" <> fg red) style choice
--------------------------------------------------------------------------------

-- Returns `Nothing` when the game is over
keepPlaying :: MonadIO m => Quarto -> MaybeT (Byline m) Quarto
keepPlaying q = do
    player <- hoistMaybe $ case Q.turn q 
        of Left  _ -> Nothing --game is over
           Right p -> Just p
    game <- case q
        of Final _                        -> hoistMaybe Nothing -- TODO print winner sequence
           Pass  q'                       -> lift $ Place <$> passTurn player q'
           Place q'@(PlaceQuarto _ piece) -> lift (placeTurn player piece q')
    keepPlaying game
    -- TODO unfinished

passTurn :: MonadIO m => Player -> PassQuarto -> Byline m PlaceQuarto
passTurn player q = (\case 
    Left e   -> sayLn (style $ show e) *> passTurn player q
    Right q' -> pure q') =<< (pass q player <$> askForPiece)

placeTurn :: MonadIO m => Player -> Piece -> PlaceQuarto -> Byline m Quarto
placeTurn player piece q = (\case 
    Left e   -> sayLn (style $ show e) *> placeTurn player piece q
    Right q' -> pure (convert q')) =<< (place q player <$> askForTile piece)
    where convert (Left  a) = Pass  a
          convert (Right a) = Final a

askForPiece :: MonadIO m => Byline m Piece
askForPiece = do
    index  <- askUser
    mPiece <- maybe askIfInvalid (pure . Just) (lookup index)
    maybe askForPiece pure mPiece
    where
        lookup s = flip M.lookup pieceMenu =<< headMay (T.toUpper s)
        askUser = ask "pass a piece: " Nothing
        askIfInvalid = lookup <$> (sayLn ("not a valid piece" <> fg red) *> askUser)

askForTile :: MonadIO m => Piece -> Byline m Tile
askForTile piece = do
    hv    <- askUser
    mTile <- maybe askIfInvalid (pure . Just) (validateTile hv)
    maybe (askForTile piece) pure mTile
    where
        askUser = ask ("where will you place " <> style piece <> " ?:") Nothing
        askIfInvalid = validateTile <$> (sayLn ("not a valid tile" <> fg red) *> askUser)

validateTile :: Text -> Maybe Tile
validateTile t = if 2 /= T.length t
    then Nothing 
    else toTile t <|> toTile (T.reverse t)
    where toTile t' = fmap (uncurry Tile) (zipR . bimap hFromChar vFromChar =<< firstTwo t')

zipR :: (Maybe a, Maybe b) -> Maybe (a, b)
zipR (Just a, Just b) = Just (a, b)
zipR _                = Nothing

firstTwo :: Text -> Maybe (Char, Char)
firstTwo t = zipR (headMay t, headMay (T.drop 1 t))

hFromChar :: Char -> Maybe HIndex
hFromChar 'A' = Just HA
hFromChar 'B' = Just HB
hFromChar 'C' = Just HC
hFromChar 'D' = Just HD
hFromChar _   = Nothing

vFromChar :: Char -> Maybe VIndex
vFromChar '1' = Just V1
vFromChar '2' = Just V2
vFromChar '3' = Just V3
vFromChar '4' = Just V4
vFromChar _   = Nothing


-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

-- | Case analysis for MaybeT
maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT mb kb (MaybeT ma) = maybe mb kb =<< ma

repeatM :: Monad m => (a -> MaybeT m a) -> a -> m a
repeatM f = g
     where g x = maybeT (pure x) g (f x)

class Style a where
    style :: a -> Stylized

instance Style Stylized where
    style = id

instance Style Piece where
    style (Piece c s h t) = color c (height h (shape s (top t))) where
        color  White  = (<> fg red)
        color  Black  = (<> fg blue <> bold)
        height Tall   = (<> underline)
        height Short  = id
        shape  Square = ("[" <>) . (<> "]")
        shape  Round  = ("(" <>) . (<> ")")
        top    Flat   = " "
        top    Hole   = stylize ('\9675' : "")

instance Style (Map Char Piece) where
    style m = pieces <> stylize "\n" <> indexes where
        pieces  = style (M.elems m)
        indexes = stylize " " <> style ((<> " ") . style . showCharNoQuotes <$> M.keys m)

instance Style Char where
    style = stylize . showCharNoQuotes

instance Style a => Style (Maybe a) where
    style (Just a) = style a
    style Nothing  = stylize ""

instance Style a => Style [a] where
    style xs = foldr ((<>) . (<> " ")) "" (style <$> xs)

instance Style Quarto where
    style q = header <> hIndex <> grid lines <> passed q where
        header = stylize $ "   :: Turn " <> show (piecesPlaced q) <> " ::\n"
        hIndex = "   " <> (" 1  2  3  4 " <> underline) <> "\n"
        grid (a, b, c, d) =
               "A |" <> rowTransform a <> "\n"
            <> "B |" <> rowTransform b <> "\n"
            <> "C |" <> rowTransform c <> "\n"
            <> "D |" <> rowTransform d <> "\n"
        rowTransform x = style $ maybe (stylize "   ") style <$> x
        passed (Place (PlaceQuarto _ p)) = "To Place: " <> style p <> "\n"
        passed _ = ""
        lines = flatten2x2 $ both halve (halve pieces)
        pieces = flip getPiece q <$> allTiles

flatten2x2 :: ((a, b), (c, d)) -> (a, b, c, d)
flatten2x2 ((w, x), (y, z)) = (w, x, y, z)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take h xs, drop h xs) where
  h = length xs `div` 2

stylize :: String -> Stylized
stylize = text . T.pack

headMay :: Text -> Maybe Char
headMay t = if T.null t then Nothing else Just (T.head t)

showCharNoQuotes :: Char -> String
showCharNoQuotes = trim . show where
    trim (_ : xs) = init xs
    trim _        = undefined -- unreachable

allPieces :: [Piece]
allPieces = [
   Piece White Square Short Hole
  ,Piece White Square Short Flat
  ,Piece White Square Tall  Hole
  ,Piece White Square Tall  Flat
  ,Piece White Round  Short Hole
  ,Piece White Round  Short Flat
  ,Piece White Round  Tall  Hole
  ,Piece White Round  Tall  Flat
  ,Piece Black Square Short Hole
  ,Piece Black Square Short Flat
  ,Piece Black Square Tall  Hole
  ,Piece Black Square Tall  Flat
  ,Piece Black Round  Short Hole
  ,Piece Black Round  Short Flat
  ,Piece Black Round  Tall  Hole
  ,Piece Black Round  Tall  Flat]

pieceMenu :: Map Char Piece
pieceMenu = M.fromList $ (toEnum <$> [65..]) `zip` allPieces