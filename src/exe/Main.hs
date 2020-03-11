{-# LANGUAGE OverloadedStrings, LambdaCase #-}

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
import Data.Bimap (Bimap, twist)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline hiding (banner, Menu)

import qualified Quarto as Q
import Quarto hiding (lines)
import Lib

--------------------------------------------------------------------------------
-- |Entry point for playing a single pass-and-play game
main :: IO ()
main = void $ runByline $ do
  let game = Q.empty
  sayLn banner
  _ <- runMaybeT (play game)
  sayLn ":::::: Thanks for playing! ::::::"
--------------------------------------------------------------------------------

-- * Functions for gameplay

-- |Text banner that says "Quarto"
banner :: Stylized
banner = (fg blue <>) . (bold <>) $ stylize $ unlines [
    "  ____                   _"
  , " / __ \\                 | |"
  , "| |  | |_   _  __ _ _ __| |_ ___"
  , "| |  | | | | |/ _` | '__| __/ _ \\"
  , "| |__| | |_| | (_| | |  | || (_) |"
  , " \\___\\_\\\\__,_|\\__,_|_|   \\__\\___/"]

-- |recursively plays the whole game from the start state to its conclusion
play :: MonadIO m => Quarto -> MaybeT (Byline m) Quarto
play game = do
    lift $ sayLn ""
    lift $ sayLn (style game)

    player <- hoistMaybe $ case Q.turn game 
        of Left  _ -> Nothing -- game is over
           Right p -> Just p

    game' <- case game
        of Final _                       -> hoistMaybe Nothing -- game is over
           Pass  q                       -> lift (Place <$> passTurn player q)
           Place q@(PlaceQuarto _ piece) -> lift (placeTurn player piece q)

    play game'

{-| 
  Play a pass turn by asking the user for a piece. If theres an exception, 
  it displays the exception and asks the user for a different input. This 
  cycle continues until a valid input is given. 
-}
passTurn :: MonadIO m => Player -> PassQuarto -> Byline m PlaceQuarto
passTurn player q = (\case 
    Left e   -> sayLn (style $ show e) *> passTurn player q
    Right q' -> pure q') =<< (pass q player <$> (sayLn (styleMenu $ availablePieceMenu (Pass q)) *> askForPiece q))

{-| 
  Play a place turn by asking the user for a tile. If theres an exception, 
  it displays the exception and asks the user for a different input. This 
  cycle continues until a valid input is given. 
-}
placeTurn :: MonadIO m => Player -> Piece -> PlaceQuarto -> Byline m Quarto
placeTurn player piece q = (\case 
    Left e   -> sayLn (style $ displayException e) *> placeTurn player piece q
    Right q' -> pure (convert q')) =<< (place 
    q player <$> askForTile piece)
    where convert (Left  a) = Pass  a
          convert (Right a) = Final a

{-|
  Asks the user for a piece. Rejects pieces that are already in play. 
  Continues to reject until a valid piece is given.
-}
askForPiece :: MonadIO m => PassQuarto -> Byline m Piece
askForPiece q = do
    index  <- askUser
    mPiece <- maybe askIfInvalid (pure . Just) (lookup index)
    maybe (sayNotValid *> askForPiece q) pure mPiece
    where
        lookup s = flip BM.lookup (availablePieceMenu (Pass q)) =<< textHeadMay (T.toUpper s)
        askUser = ask "pass a piece: " Nothing
        askIfInvalid = lookup <$> (sayNotValid *> askUser)
        sayNotValid = sayLn ("not a valid piece" <> fg red)

{-|
  Asks the user for a tile. Rejects tiles that are occupied. 
  Continues to reject until a valid tile is given.
-}
askForTile :: MonadIO m => Piece -> Byline m Tile
askForTile piece = do
    hv    <- askUser
    mTile <- maybe askIfInvalid (pure . Just) (validateTile hv)
    maybe (sayNotValid *> askForTile piece) pure mTile
    where
        askUser = ask ("where will you place " <> style piece <> "?: ") Nothing
        askIfInvalid = validateTile <$> (sayNotValid *> askUser)
        sayNotValid = sayLn ("not a valid tile" <> fg red)

{-|
  Parses a string into a Tile if possible. If the given version doesn't parse, 
  it attempts the reverse of the string.
-}
validateTile :: Text -> Maybe Tile
validateTile t = if 2 /= T.length t
    then Nothing 
    else toTile tCap <|> toTile (T.reverse tCap)
    where toTile t' = fmap (uncurry Tile) (uncurry crossProduct . bimap hFromChar vFromChar =<< firstTwo t')
          tCap = T.toUpper t

{-|
  A map of pieces not used in a given game state, where the keys are Char indicies 
  that the user uses as input.
-}
availablePieceMenu :: Quarto -> Bimap Char Piece
availablePieceMenu q = twist $ foldr BM.delete (twist pieceMenu) (unavailablePieces q)

-- |Parses a character to a horizontal index
hFromChar :: Char -> Maybe HIndex
hFromChar 'A' = Just HA
hFromChar 'B' = Just HB
hFromChar 'C' = Just HC
hFromChar 'D' = Just HD
hFromChar _   = Nothing

-- |Parses a character to a vertical index
vFromChar :: Char -> Maybe VIndex
vFromChar '1' = Just V1
vFromChar '2' = Just V2
vFromChar '3' = Just V3
vFromChar '4' = Just V4
vFromChar _   = Nothing

-- |Converts String to Stylized
stylize :: String -> Stylized
stylize = text . T.pack

-- |The piece menu with all pieces available
pieceMenu :: Bimap Char Piece
pieceMenu = BM.fromList $ (toEnum <$> [65..]) `zip` allPieces

-- |Takes a list and a separator and converts the list to Stylized
styleListSep :: Style a => Stylized -> [a] -> Stylized
styleListSep sep as = style (intersperse sep $ style <$> as)

-- |Converts the menu to Stylized so it can be displayed to the user
styleMenu :: Bimap Char Piece -> Stylized
styleMenu m = pieces <> "\n" <> indexes where
        pieces  = styleListSep " " (BM.elems m)
        indexes = " " <> styleListSep "   " (showCharNoQuotes <$> BM.keys m)

-- * class and instances for working with @Stylized@

-- |class @Style@ provides function @style@ for getting Stylized representation
class Style a where
    style :: a -> Stylized

-- |identity instance. All values of type Stylized can be converted to themselves.
instance Style Stylized where
    style = id

-- |instance for lists has no separator
instance Style a => Style [a] where
    style = foldr ((<>) . style) ""

-- |instance for Char removes quotes from show
instance Style Char where
    style = stylize . showCharNoQuotes

{-|
  instance for piece:
  Color:  Red        | Bold & Blue
  Height: Underlined | Not Underlined
  Shape:  [ ]        | ( )
  Hole    Circle     | No Circle
-}
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

-- |instance for Player reuses show: P1, P2
instance Style Player where
    style = stylize . show

{-|
  instance for Quarto:
  - Header during game includes player, and turn number
  - Header at game termination includes winning player or tie declaration
  - Vertical and Horizontal index markers
  - 4x4 grid with all placed tiles
  - On place turns, indicates the piece that is to be placed

  example:
  @
   :: P2 Turn 2 ::
    1   2   3   4 
A |               
B |    [○]        
C |               
D |            ( )
To Place: [ ]
  @
-}
instance Style Quarto where
    style q = header <> hIndex <> grid lines <> passed q where
        header = "   :: " <> headerText <> " ::\n"
        hIndex = "   " <> (" 1   2   3   4 " <> underline) <> "\n"
        grid (a, b, c, d) =
                "A |" <> rowTransform a <> "\n"
            <> "B |" <> rowTransform b <> "\n"
            <> "C |" <> rowTransform c <> "\n"
            <> "D |" <> rowTransform d <> "\n"
        rowTransform x = styleListSep " " $ maybe (stylize "   ") style <$> x
        passed (Place (PlaceQuarto _ p)) = "To Place: " <> style p <> "\n"
        passed _ = ""
        lines = flatten2x2 $ both halve (halve pieces)
        pieces = flip getPiece q <$> allTiles
        headerText = fromMaybe "" $ 
            winOrTie <|> fmap ((<> " Turn " <> stylize (show (piecesPlaced q))) . style) (eitherToMaybe $ Q.turn q)
        winOrTie = finalState q >>= (\case 
            Tie          -> Just "Tie Game!"
            (Winner p _) -> Just (style p <> " Wins!"))