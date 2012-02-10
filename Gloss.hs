module Gloss 
   ( module Graphics.Gloss.Interface.Game
   , DPixel
   , translate
   , pixel
   , line
   , circle
   ) where


import Graphics.Gloss.Interface.Game hiding (Vector, translate, line, circle)
import qualified Graphics.Gloss.Interface.Game as GL

import Numeric.NumType

import Numeric.Units.Dimensional.Extensible
import Numeric.Units.Dimensional

import DimensionalVector

newtype TPixel = TPixel TPixel
type DPixel = DExt TPixel Pos1 DOne

pixel :: Num a => Unit DPixel a
pixel = Dimensional 1

translate :: Vector DPixel -> Picture -> Picture
translate (x, y) = Translate (realToFrac $ x /~ pixel) (realToFrac $ y /~ pixel)

line :: [Vector DPixel] -> Picture
line = GL.line . map (mapV (realToFrac . (/~ pixel)))

circle :: Scalar DPixel -> Picture
circle = GL.circle . (realToFrac . (/~ pixel))
