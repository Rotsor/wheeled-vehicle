{-# LANGUAGE TemplateHaskell #-}
module Player where

import Common
import Prelude()

type Cooldown = Time

data Player = Player {
  _position :: Position
  , _velocity :: Velocity
  , _direction :: Direction
  , _cooldown :: Cooldown
  } deriving Show

friction = 100 *~ newton
engineForce = 100 *~ newton
enginePower = 1000 *~ watt

shipMass = 1 *~ kilo gram

step :: Time -> InputState -> Player -> Player
step dt input (Player pos speed dir cooldown) = 
   Player
   (pos `addPV` mulSV dt speed)
   (speed `addV` mulSV dt accel)
   (dir + dt * angleSpeed)
   (cooldown - dt)
   where
  accel :: Vector DAcceleration
  accel | isDown 's' input = negate friction `mulSV` normZV speed `divVS` shipMass
        | isDown 'w' input = limitPower enginePower (mulSV engineForce (directionV dir)) speed `divVS` shipMass
        | otherwise = (0 *~ (meter / second / second),0 *~ (meter / second / second))

  angleSpeed | isDown 'd' input = (-5) *~ (radian / second)
             | isDown 'a' input = 5 *~ (radian / second)
             | otherwise = 0 *~ (radian / second)

$(mkLabels [''Player])
