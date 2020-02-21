{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Quarto

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Byline

--------------------------------------------------------------------------------
main :: IO ()
main = void $ runByline $ do

  sayLn ("Piece WQTF = " <> draw (Piece White Square Tall Flat))
  sayLn ("Piece BRSH = " <> draw (Piece Black Round Short Hole))

  let question = "What " <> ("piece" <> bold) <> " do you want to see? "
  piece <- ask question Nothing

  sayLn $ maybe ("not a valid piece" <> bg red) draw (readPiece $ T.unpack piece)

--------------------------------------------------------------------------------

draw :: Piece -> Stylized
draw (Piece c s h t) = color c (height h (shape s (top t))) where
    color  White  x = x <> fg black <> bg yellow
    color  Black  x = x <> fg blue
    height Tall   x = x <> bold
    height Short  x = x
    shape  Square x = "[" <> x <> "]"
    shape  Round  x = "(" <> x <> ")"
    top    Flat     = " "
    top    Hole     = text $ T.pack ('\9675' : "")

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