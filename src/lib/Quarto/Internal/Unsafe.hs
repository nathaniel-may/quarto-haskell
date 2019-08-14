module Quarto.Internal.Unsafe where

import Quarto.Types (Piece, Board, GameEnd)


-- game logic assumes these types have only been constructed via their associated
-- smart constructors. Using them directly may cause incorrect behavior.
newtype PassQuarto  = MkPassQuarto  Board         deriving (Eq, Show, Read)
data    PlaceQuarto = MkPlaceQuarto Board Piece   deriving (Eq, Show, Read)
data    FinalQuarto = MkFinalQuarto Board GameEnd deriving (Eq, Show, Read)

