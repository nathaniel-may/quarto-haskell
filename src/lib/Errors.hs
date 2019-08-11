module Errors where

import qualified Data.Text as T
import Data.Text (Text)

newtype Err = Err Text
            deriving (Eq, Show, Read)

err :: String -> Err
err = Err . T.pack