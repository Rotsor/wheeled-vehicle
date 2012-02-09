module Position
  ( Position(unPosition)
  , mkPosition
  , addPV
  , worldWidth
  , worldHeight
  , ($+|)
  )
  where

import Math
import qualified Prelude as P
import Numeric.Units.Dimensional (Dimensional(..))

worldWidth = 800 *~ meter
worldHeight = 600 *~ meter

newtype Position = Position { unPosition :: Vector DLength } deriving Show

mkPosition :: Vector DLength -> Position
mkPosition (x,y) = Position (fMod x worldWidth, fMod y worldHeight) where
  infixl 7 `fMod`
  fMod :: Scalar a -> Scalar a -> Scalar a
  fMod (Dimensional x) (Dimensional m) = Dimensional $ x P.- P.fromIntegral (P.floor (x P./ m)) P.* m

infixl 6 `addPV`, $+|
addPV :: Position -> Vector DLength -> Position
addPV (Position p) v = mkPosition (p `addV` v)

($+|) = addPV
