{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Car(Car, step, draw, TrueCar(Car)) where

import Common
import Gloss
import Position
import Prelude()
import Data.Label
import Data.List
import Control.Arrow((&&&))

import Numeric.NumType (Zero, Pos1, Pos2)

data TrueCar = Car
 { _position :: Position
 , _velocity :: Velocity
 , _direction :: Direction
 , _angularVelocity :: Scalar DAngularVelocity
 } deriving (Show)
$(mkLabels [''TrueCar])

type DbgCar = (TrueCar, InputState)
type Car = DbgCar

carLength = 50 *~ meter
halfLength = carLength / (2 *~ one)

front car = halfLength `mulSV` directionV (get direction car)
rear car = negate halfLength `mulSV` directionV (get direction car)

lineM = line . map (mapV toPixels)

friction = 100 *~ newton
enginePower = 10000 *~ watt

mass = 1 *~ kilo gram

type DMomentOfInertia = Dim Pos2 Pos1 Zero Zero Zero Zero Zero

inertia :: Scalar DMomentOfInertia
inertia = carLength * carLength * mass / (12 *~ one)

magic :: (Mul ad DLength d, Mul d DOne d) => Vector d -> Scalar DPlaneAngle -> Scalar ad -> (Vector d, Vector d)
magic center angle amount = mapV ((center |+|) . (amount * halfLength *|) . rotateV (pi / _2)) (directionV angle, mapV negate $ directionV angle)

accel :: Vector DForce -> Vector DForce -> Vector DAcceleration
accel f g = (f |+| g) |/ mass

aaccel :: Vector DForce -> Vector DForce -> TrueCar -> Scalar DAngularAcceleration
aaccel f g car = torque / inertia where
  torque = front car `cross` f + rear car `cross` g

rawStep :: Time -> Vector DForce -> Vector DForce -> TrueCar -> TrueCar
rawStep dt f g car@(Car x xv a av) =
   Car
   (x $+| dt *| xv)
   (xv |+| dt *| accel f g)
   (a + dt * av)
   (av + dt * aaccel f g car) where

v2l :: (a, a) -> [a]
v2l (x,y) = [x,y]

type Stat = (Position, Vector DVelocity, Vector DAcceleration, Vector DForce)

getStats :: Vector DForce -> Vector DForce -> TrueCar -> [(Position, Vector DVelocity, Vector DAcceleration, Vector DForce)]
getStats f g car@(Car x xv a av) = zip4 
     (map (mkPosition . (unPosition x |+|) . (halfLength *|)) [directionV a, mapV negate $ directionV a])
     (v2l $ magic xv a av) (v2l $ magic (accel f g) a (aaccel f g car)) [f, g]

drawStats :: [Stat] -> Picture
drawStats = Pictures . map drawStat where
 drawStat :: Stat -> Picture
 drawStat (pos, vel, accel, force) = Pictures $ map draw
  [ (yellow, vel |* (0.5 *~ second))
  , (blue, force |/ (100 *~ newton) |* (50 *~ meter))
  , (red, accel |* (0.5 *~ (second * second)))
  ] where
  draw (color, vector) = drawAt pos $ Color color $ lineM [zeroV, vector]

rotateCar :: Scalar DPlaneAngle -> TrueCar -> TrueCar
rotateCar da (Car x xv a av) = Car x (rotateV da xv) (a + da) av

limitMagnitude mag v | mv > mag = v |/ (mag / mv)
 | otherwise = v where
  mv = magV v

-- we rotate the world so that the car stays along the "y" axis with its 
-- front pointing in the positive direction.
-- We want to find a force for the rear wheel such that the acceleration of rear wheel along the "x" axis stays 0
-- So we need to solve a_rear_x = 0
-- a_rear_x = a_x + aa * r
-- a_x = (f_x + g_x) / m
-- aa = torque / inertia
-- torque = front car `cross` f + rear car `cross` g = f_x * (-r) + g_x * r
-- 
-- Solving for g_x:
-- a_x + aa * r = 0
-- (f_x + g_x) / m + torque / inertia * r = 0
-- (f_x + g_x) / m + r * r * (g_x - f_x) / inertia = 0
-- let m * r * r / inertia = xx
-- (f_x + g_x) + xx * (g_x - f_x) = 0
-- f_x * (1 - xx) + g_x (1 + xx) = 0
-- g_x = - f_x * (1 - xx) / (1 + xx)

-- correction: instead of a_rear_x = 0 we must solve a_rear_x = - v_y * angularVelocity
-- (f_x + g_x) / m + r * r * (g_x - f_x) / inertia = - v_y * angularVelocity
-- f_x + g_x + xx * (g_x - f_x) = - m * v_y * angularVelocity
-- let yy = m * v_y * angularVelocity
-- g_x (1 + xx) = - (m * v_y * angularVelocity + f_x * (1 - xx))
getForces' :: (Scalar DForce, Scalar DForce) -> TrueCar -> (Vector DForce, Vector DForce)
getForces' (steer, drive) (Car x v@(_,vy) _ av) = (ff, rf) where
  (fx, fy) = ff
  vf = v |+| r * av *| (negate _1, zero)
  dirf = normZV vf
  ff = limitMagnitude friction $ steer *| (rotateV (pi / _2) dirf)
  
  -- max g_y square allowed by friction
  mgy2 = max zero $ friction * friction - rearSideForce * rearSideForce
  mgyp | vy > zero = min (enginePower / vy)
       | vy < zero = max (enginePower / vy)
       | otherwise = id
  gy = mgyp $ min (sqrt mgy2) $ max (negate $ sqrt mgy2) $ drive
  
  rf = (rearSideForce, gy)
  
  r = halfLength
  rearSideForce = gx where
    i = inertia
    m = mass
    xx = r * r * m / i
    gx = negate (fx * (_1 - xx) + m * vy * av) / (xx + _1)

gf controls car = mapV (rotateV a) . getForces' controls . rotateCar (negate a) $ car where
  a = get direction car - pi / _2
    
getForces car input = gf ff car where
  ff = controlForce "tghf"
  controlForce = 
     foldr (|+|) zeroV
     . map fst
     . filter ((`isDown` input) . snd)
     . zip [(zero, 100 *~ newton)
           , (zero, (-100) *~ newton)
           , ((-100) *~ newton, zero)
           , (100 *~ newton, zero)
           ]

step :: Time -> InputState -> DbgCar -> DbgCar
step dt input (car, _) = (rawStep (dt/(1 *~ one)) ff rf car, input)
   where
   (ff, rf) = getForces car input
    

draw :: DbgCar -> Picture
draw (car, lastInput) = Pictures [drawStats . ($ car) . uncurry getStats $ getForces car lastInput, 
  drawAt (get position car) $
  Pictures $
    [ Color (makeColor 1 1 1 0.4) $ lineM [rear car, front car]
--    , drawWheel (front car) (rotationVelocity halfLength (get angularVelocity car) (get direction car))
--    , drawWheel (rear car) (rotationVelocity halfLength (get angularVelocity car) (pi + get direction car))
    ]
  ] where
    drawWheel pos vel = 
       Color yellow 
         $ lineM
         $ map ((pos `addV`) . (0.1 *~ second `mulSV`))
         $ [zeroV, 
            get velocity car 
            `addV` vel]
