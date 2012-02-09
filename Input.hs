module Input(InputState, isDown) where

import Data.Map(Map)
import qualified Data.Map as Map

type InputState = Map Char Bool

isDown ch m = maybe False id (Map.lookup ch m)
