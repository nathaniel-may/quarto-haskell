module Quarto.Types (module SafeTypes) where

-- Hides hidden constructors
import Quarto.Types.Internal as SafeTypes hiding (
    MkBoard
  , MkPassQuarto
  , MkPlaceQuarto
  , MkFinalQuarto)