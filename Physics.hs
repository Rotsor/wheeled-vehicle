module Physics where

import Math
import Prelude()

limitPower :: Scalar DPower -> Vector DForce -> Vector DVelocity -> Vector DForce
limitPower max force speed = mulSV coeff force where
  coeff | power > max = max / power
        | otherwise = 1 *~ one
  power = dotV force speed

rotationVelocity :: Scalar DLength -> -- ^ Radius
                    Scalar DAngularVelocity -> -- ^ Counter-clockwise Rotation speed, in radians per second
                    Scalar DPlaneAngle -> -- ^ Angle to the point of interest
                    Vector DVelocity -- ^ The speed of the point of interest
rotationVelocity r v a = v * r `mulSV` rotate90ccw (directionV a)
