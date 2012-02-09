module Math
  ( module NP
  , module DimensionalVector
  , zero
  , zeroV
  )  where

import Numeric.Units.Dimensional.Prelude as NP hiding(Time, Velocity, AngularVelocity, zero)
import DimensionalVector
import Numeric.Units.Dimensional(Dimensional(..))

zero :: Scalar d
zero = Dimensional 0
zeroV :: Vector d
zeroV = (zero, zero)

