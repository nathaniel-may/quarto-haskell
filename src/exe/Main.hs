{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Quarto

import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do

  sayLn "Piece Menu: "
  sayLn (drawMenu pieceMenu)

--   let question = "What " <> ("piece" <> bold) <> " do you want to see? "
--   piece <- ask question Nothing

--   sayLn $ maybe ("not a valid piece" <> bg red) draw (readPiece $ T.unpack piece)
--------------------------------------------------------------------------------

stylize :: String -> Stylized
stylize = text . T.pack

draw :: Piece -> Stylized
draw (Piece c s h t) = color c (height h (shape s (top t))) where
    color  White  x = x <> fg red
    color  Black  x = x <> fg blue <> bold
    height Tall   x = x <> underline
    height Short  x = x
    shape  Square x = "[" <> x <> "]"
    shape  Round  x = "(" <> x <> ")"
    top    Flat     = " "
    top    Hole     = stylize ('\9675' : "")

-- this is a naive and error prone implementation, but it's going to go away
-- eventually these will be keyed in by numbers
readPiece :: String -> Maybe Piece
readPiece ['W', 'Q', 'S', 'H'] = Just $ Piece White Square Short Hole
readPiece ['W', 'Q', 'S', 'F'] = Just $ Piece White Square Short Flat
readPiece ['W', 'Q', 'T', 'H'] = Just $ Piece White Square Tall  Hole
readPiece ['W', 'Q', 'T', 'F'] = Just $ Piece White Square Tall  Flat
readPiece ['W', 'R', 'S', 'H'] = Just $ Piece White Round  Short Hole
readPiece ['W', 'R', 'S', 'F'] = Just $ Piece White Round  Short Flat
readPiece ['W', 'R', 'T', 'H'] = Just $ Piece White Round  Tall  Hole
readPiece ['W', 'R', 'T', 'F'] = Just $ Piece White Round  Tall  Flat
readPiece ['B', 'Q', 'S', 'H'] = Just $ Piece Black Square Short Hole
readPiece ['B', 'Q', 'S', 'F'] = Just $ Piece Black Square Short Flat
readPiece ['B', 'Q', 'T', 'H'] = Just $ Piece Black Square Tall  Hole
readPiece ['B', 'Q', 'T', 'F'] = Just $ Piece Black Square Tall  Flat
readPiece ['B', 'R', 'S', 'H'] = Just $ Piece Black Round  Short Hole
readPiece ['B', 'R', 'S', 'F'] = Just $ Piece Black Round  Short Flat
readPiece ['B', 'R', 'T', 'H'] = Just $ Piece Black Round  Tall  Hole
readPiece ['B', 'R', 'T', 'F'] = Just $ Piece Black Round  Tall  Flat
readPiece _ = Nothing

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

showCharNoQuotes :: Char -> String
showCharNoQuotes = trim . show where
    trim (_ : xs) = init xs
    trim _        = undefined -- unreachable

drawMenu :: Map Char Piece -> Stylized
drawMenu m = pieces <> stylize "\n" <> indexes where
    pieces  = foldr ((<>) . (<> " ")) "" (draw <$> M.elems m)
    indexes = stylize $ foldr ((<>) . (<> "  ") . (" " <>)) "" (showCharNoQuotes <$> M.keys m)