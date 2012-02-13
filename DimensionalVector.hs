{-# LANGUAGE FlexibleContexts #-}
module DimensionalVector
   ( Scalar
   , Vector
   , directionV
   , mulSV
   , divVS
   , magV
   , normaliseV
   , normZV
   , rotateV
   , rotate90ccw
   , dotV
   , addV
   , mapV
   , (|+|)
   , (*|)
   , (|*)
   , (|/)
   , cross
   , atan2V
   ) where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude 
import Numeric.Units.Dimensional
import Numeric.NumType(Pos2)

type Scalar d = Quantity d Double
type Vector d = (Scalar d, Scalar d)


infixl 7 `mulSV`
infixl 6 `addV`

mapV f (x, y) = (f x, f y)

directionV :: Scalar DOne -> Vector DOne
directionV dir = rotateV dir (1 *~ one, 0 *~ one)

mulSV :: Mul a b c => Scalar a -> Vector b -> Vector c
mulSV c = mapV (c *)

divVS :: Div a b c => Vector a -> Scalar b -> Vector c
l `divVS` d = mapV (/ d) l

magV :: (Mul d d sq, Root sq Pos2 d, Floating a) => (Quantity d a, Quantity d a) -> Quantity d a
magV (x, y) = sqrt $ x * x + y * y

normaliseV :: (Mul d d sq, Root sq Pos2 d, Div d d one) => Vector d -> Vector one
normaliseV l = l `divVS` magV l

normZV :: (Mul d d sq, Root sq Pos2 d, Div d d one) => Vector d -> Vector one
normZV v | magV v < Dimensional 1e-10 = (Dimensional 0,Dimensional 0)
         | otherwise = normaliseV v

rotateV :: Mul d DOne d => Scalar DOne -> Vector d -> Vector d
rotateV a (x, y) = (x * cos a - y * sin a, y * cos a + x * sin a)

rotate90ccw :: Vector d -> Vector d
rotate90ccw (x, y) = (negate y, x)

dotV (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

addV :: Vector a -> Vector a -> Vector a
addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

atan2V (x, y) = atan2 y x

infixl 6 |+|
infixl 7 *|, |/, |*

(|+|) = addV

(|*) :: Mul b a c => Vector b -> Scalar a -> Vector c
v |* c = mapV (* c) v

(*|) :: Mul a b c => Scalar a -> Vector b -> Vector c
(*|) = mulSV

(|/) :: Div a b c => Vector a -> Scalar b -> Vector c
(|/) = divVS

cross :: Mul a b c => Vector a -> Vector b -> Scalar c
(x1, y1) `cross` (x2, y2) = x1 * y2 - y1 * x2
