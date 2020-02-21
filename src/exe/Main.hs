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

  let question = "choose a piece: "
  input <- ask question Nothing

  let choice = flip M.lookup pieceMenu =<< headMay input
  sayLn $ maybe ("not a valid piece" <> fg red) draw choice
--------------------------------------------------------------------------------

stylize :: String -> Stylized
stylize = text . T.pack

headMay :: Text -> Maybe Char
headMay t = if T.null t then Nothing else Just (T.head t)

draw :: Piece -> Stylized
draw (Piece c s h t) = color c (height h (shape s (top t))) where
    color  White  = (<> fg red)
    color  Black  = (<> fg blue <> bold)
    height Tall   = (<> underline)
    height Short  = id
    shape  Square = ("[" <>) . (<> "]")
    shape  Round  = ("(" <>) . (<> ")")
    top    Flat   = " "
    top    Hole   = stylize ('\9675' : "")

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