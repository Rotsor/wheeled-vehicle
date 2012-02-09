module Viewport where

import Math
import Gloss
import Prelude((.))
import Position

toPixels = (*~ pixel) . (/~ meter)
fromPixels = (*~ meter) . (/~ pixel)

drawAt :: Position -> Picture -> Picture
drawAt pos img = translate (mapV toPixels (unPosition pos)) $ Pictures 
       $ [ translate ((dx *~ one) * toPixels worldWidth, (dy *~ one) * toPixels worldHeight) img | dx <- [-1..1], dy <- [-1..1]]
