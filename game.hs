{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Common hiding(id, (.))
import Control.Category
import Control.Arrow hiding(second)
import qualified Data.Map as Map
import Control.Monad.Reader
import qualified Prelude as P
import Car(Car(..), TrueCar(..))
import qualified Car
import Player(Player(..))
import qualified Player

import Gloss

type Owner = ()
data Projectile = 
     Mine Position Time Owner
   | Bullet Position Velocity Time deriving Show

data GameState = GameState 
  { _players :: [Player]
  , _cars :: [Car]
  , _projectiles :: [Projectile]
  } deriving (Show)

data World = World
  { _gameState :: GameState
  , _inputState :: InputState
  , _debugInfo :: String
  }

$(mkLabels [''World, ''GameState])


projectileLifetime :: Scalar DTime
projectileLifetime = 2.5 *~ second

data Weapon = MineLayer | Machinegun

weaponShoot :: Weapon -> Player -> (Player, [Projectile])
weaponShoot _ pl | get Player.cooldown pl > zero = (pl, [])
weaponShoot MineLayer pl = (set Player.cooldown (2 *~ second) pl
    , [Mine (get Player.position pl) projectileLifetime ()])
weaponShoot Machinegun pl = (set Player.cooldown (0 *~ second) pl
    , [Bullet (get Player.position pl) 
    (get Player.velocity pl `addV` ((200 *~ (meter / second)) `mulSV` directionV (get Player.direction pl))) 
    projectileLifetime 
    ])


shoot :: InputState -> GameState -> GameState
shoot input =
    when (isDown 'x' input) (go MineLayer)
    . when (isDown 'z' input) (go Machinegun)
  where
    go wpn w = set players newPlayers . modify projectiles (concat newProjectiles ++) $ w where
      (newPlayers, newProjectiles) = unzip $ map (weaponShoot wpn) (get players w)
    when True f x = f x
    when False _ x = x

projTime :: Projectile :-> Time
projTime = lens g s where
  g (Bullet _ _ t) = t
  g (Mine _ t _) = t
  
  s t (Bullet x y _) = Bullet x y t
  s t (Mine x _ y) = Mine x t y

projectileStep :: Time -> Projectile -> [Projectile]
projectileStep dt = age dt . move where
  move (Bullet x v t) = (Bullet (x `addPV` dt `mulSV` v) v t)
  move other = other
  
  age dt proj | newTime < 0 *~ second = []
    | otherwise = [set projTime newTime proj]
    where
     newTime = get projTime proj - dt
  
worldStep :: Time -> InputState -> GameState -> GameState
worldStep dt input = 
  modify cars (map (Car.step dt input)) .
  modify players (map (Player.step dt input)) .
  modify projectiles (concatMap (projectileStep dt)) .
  shoot input

drawPlayer :: Picture -> Player -> Picture
drawPlayer img (Player pos _ dir _) = drawAt pos $ Rotate (realToFrac $ negate dir /~ degree) img

drawProjectile :: Projectile -> Picture
drawProjectile (Bullet pos speed t) = drawAt pos 
    $ Color (makeColor 1 1 0 (realToFrac $ t / projectileLifetime /~ one))
    $ line (map (mapV (toPixels)) [(0 *~ meter,0 *~ meter), 0.1 *~ second `mulSV` speed])
drawProjectile (Mine pos t owner) = 
    drawAt pos
    . Color (makeColor 0 1 0 (realToFrac $ t / projectileLifetime /~ one))
    $ circle (5 *~ pixel)
    
drawWorld img w = Pictures $
  map (drawPlayer img) (get players w)
  ++ map drawProjectile (get projectiles w)
  ++ map Car.draw (get cars w)

processEventDbg event = set debugInfo (show event)

processEvent (EventKey (Char ch) state _ _)
    = modify inputState (Map.insertWith const ch (state == Down))
processEvent _ = id

initialWorld = World (GameState {
  _players = 
    [ Player (mkPosition zeroV) zeroV zero zero
    , Player (mkPosition (400*~meter,zero)) zeroV (pi / (2 *~ one)) zero
    ]
  , _projectiles = []
  , _cars = 
    [ (Car (mkPosition zeroV) (000 *~ (meter / second), zero) (pi / _2) zero, Map.empty)
    , (Car (mkPosition (200*~meter,zero)) ((-20) *~ (meter / second), zero) (pi / _2) (1 *~ (one / second)), Map.empty)
    ]
  }) Map.empty "nothing happened"

main = do
  img <- fmap (Rotate 90) (loadBMP "fighter.bmp")
  play (InWindow "Qwe" (800, 600) (20, 20)) (makeColor 0.2 0.2 0.2 1) 100 initialWorld 
   (fmap Pictures (
      sequence
      [ Scale 0.1 0.1 . Text . get debugInfo
      , Translate 0 50 . Scale 0.1 0.1 . Text . show . get gameState
      , drawWorld img . get gameState
      ]))
   (fmap (foldr (.) id) (sequence [processEvent, processEventDbg]))
   (\t world -> modify gameState (worldStep (realToFrac t *~ second) (get inputState world)) world)
