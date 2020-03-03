{-# LANGUAGE OverloadedStrings, FlexibleInstances, LambdaCase #-}

module Main (main) where

import Prelude hiding (lookup, lines)

import Control.Applicative ((<|>))
import Control.Exception (displayException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Bifunctor (bimap)
import qualified Data.Bimap as BM
import Data.Bimap (Bimap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline hiding (banner, Menu)

import qualified Quarto as Q
import Quarto hiding (lines)
import Lib

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do
  let game = Q.empty
  sayLn banner
  _ <- runMaybeT (play game)
  sayLn ":::::: Thanks for playing! ::::::"
--------------------------------------------------------------------------------

banner :: Stylized
banner = (fg blue <>) . (bold <>) $ stylize $ unlines [
    "  ____                   _"
  , " / __ \\                 | |"
  , "| |  | |_   _  __ _ _ __| |_ ___"
  , "| |  | | | | |/ _` | '__| __/ _ \\"
  , "| |__| | |_| | (_| | |  | || (_) |"
  , " \\___\\_\\\\__,_|\\__,_|_|   \\__\\___/"]

-- recursively plays the whole game from the start state to its conclusion
play :: MonadIO m => Quarto -> MaybeT (Byline m) Quarto
play game = do
    lift $ sayLn ""
    lift $ sayLn (style game)

    player <- hoistMaybe $ case Q.turn game 
        of Left  _ -> Nothing -- game is over
           Right p -> Just p

    game' <- case game
        of Final _                       -> hoistMaybe Nothing -- game is over
           Pass  q                       -> lift $ Place <$> passTurn player q
           Place q@(PlaceQuarto _ piece) -> lift (placeTurn player piece q)

    play game'

passTurn :: MonadIO m => Player -> PassQuarto -> Byline m PlaceQuarto
passTurn player q = (\case 
    Left e   -> sayLn (style $ show e) *> passTurn player q
    Right q' -> pure q') =<< (pass q player <$> (sayLn (style $ availablePieceMenu (Pass q)) *> askForPiece q))

placeTurn :: MonadIO m => Player -> Piece -> PlaceQuarto -> Byline m Quarto
placeTurn player piece q = (\case 
    Left e   -> sayLn (style $ displayException e) *> placeTurn player piece q
    Right q' -> pure (convert q')) =<< (place 
    q player <$> askForTile piece)
    where convert (Left  a) = Pass  a
          convert (Right a) = Final a

askForPiece :: MonadIO m => PassQuarto -> Byline m Piece
askForPiece q = do
    index  <- askUser
    mPiece <- maybe askIfInvalid (pure . Just) (lookup index)
    maybe (sayNotValid *> askForPiece q) pure mPiece
    where
        lookup s = flip BM.lookup (availablePieceMenu (Pass q)) =<< headMay (T.toUpper s)
        askUser = ask "pass a piece: " Nothing
        askIfInvalid = lookup <$> (sayNotValid *> askUser)
        sayNotValid = sayLn ("not a valid piece" <> fg red)

askForTile :: MonadIO m => Piece -> Byline m Tile
askForTile piece = do
    hv    <- askUser
    mTile <- maybe askIfInvalid (pure . Just) (validateTile hv)
    maybe (sayNotValid *> askForTile piece) pure mTile
    where
        askUser = ask ("where will you place " <> style piece <> "?: ") Nothing
        askIfInvalid = validateTile <$> (sayNotValid *> askUser)
        sayNotValid = sayLn ("not a valid tile" <> fg red)

validateTile :: Text -> Maybe Tile
validateTile t = if 2 /= T.length t
    then Nothing 
    else toTile tCap <|> toTile (T.reverse tCap)
    where toTile t' = fmap (uncurry Tile) (zipR . bimap hFromChar vFromChar =<< firstTwo t')
          tCap = T.toUpper t

availablePieceMenu :: Quarto -> Bimap Char Piece
availablePieceMenu q = invertMap $ foldr BM.delete (invertMap pieceMenu) (unavailablePieces q)

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

finalState :: Quarto -> Maybe GameEnd
finalState = \case
    Final (FinalQuarto _ Tie) -> Just Tie
    Final (FinalQuarto _ win) -> Just win
    _ -> Nothing

stylize :: String -> Stylized
stylize = text . T.pack

pieceMenu :: Bimap Char Piece
pieceMenu = BM.fromList $ (toEnum <$> [65..]) `zip` allPieces

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

instance Style Player where
    style = stylize . show

instance Style (Bimap Char Piece) where
    style m = pieces <> stylize "\n" <> indexes where
        pieces  = style (BM.elems m)
        indexes = stylize " " <> style ((<> " ") . style . showCharNoQuotes <$> BM.keys m)

instance Style Char where
    style = stylize . showCharNoQuotes

instance Style a => Style [a] where
    style xs = foldr ((<>) . (<> " ")) "" (style <$> xs)

instance Style Quarto where
    style q = header <> hIndex <> grid lines <> passed q where
        header = "   :: " <> headerText <> " ::\n"
        hIndex = "   " <> (" 1   2   3   4 " <> underline) <> "\n"
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
        headerText = fromMaybe "" $ 
            winOrTie <|> fmap ((<> " Turn " <> stylize (show (piecesPlaced q))) . style) (eitherToMaybe $ Q.turn q)
        winOrTie = (\case 
            Tie          -> Just "Tie Game!"
            (Winner p _) -> Just (style p <> " Wins!")) =<< finalState q