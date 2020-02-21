{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main (main) where

import Quarto


import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Console.Byline hiding (Menu)

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do

  sayLn "Empty Game: "
  sayLn (style Quarto.empty)

--   sayLn "Non Empty Game: "
--   sayLn (style Quarto.empty)

  sayLn (style pieceMenu)

  let question = "pass a piece: "
  input <- ask question Nothing

  let choice = flip M.lookup pieceMenu =<< headMay (T.toUpper input)
  sayLn $ maybe ("not a valid piece" <> fg red) style choice
--------------------------------------------------------------------------------

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
        hIndex = "   " <> (" A  B  C  D " <> underline) <> "\n"
        grid (a, b, c, d) =
               "1 |" <> rowTransform a <> "\n" 
            <> "2 |" <> rowTransform b <> "\n" 
            <> "3 |" <> rowTransform c <> "\n" 
            <> "4 |" <> rowTransform d <> "\n"
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