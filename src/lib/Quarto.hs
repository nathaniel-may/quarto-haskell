module Quarto where


data Color  = Black | White
data Shape  = Round | Square
data Height = Tall  | Short
data Top    = Flat  | Hole

data Piece = Color Shape Height Top

data Index = I1 | I2 | I3 | I4