{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bug where

import qualified Prelude as P
import Prelude(undefined, Float, Num)

data Zero
data Pos n
type Pos1 = Pos Zero

class NumTypeI n where toNum :: (Num a) => n -> a

instance NumTypeI Zero where toNum _ = 0
instance PosTypeI Zero
instance (PosTypeI n) => PosTypeI (Pos n)

instance (PosTypeI n) => NumTypeI (Pos n) where
   toNum _ = toNum (undefined :: n) P.+ 1

class (NumTypeI a, NumTypeI b) => Succ a b | a -> b, b -> a
instance Succ Zero (Pos Zero)
instance (PosTypeI a) => Succ (Pos a) (Pos (Pos a))

class (NumTypeI a, NumTypeI b, NumTypeI c) => Add a b c | a b -> c
instance (PosTypeI a, Succ b c, Add a c d) => Add (Pos a) b d
instance (NumTypeI a) => Add Zero a a

class (NumTypeI a, NumTypeI b, NumTypeI c) => Sub a b c | a b -> c
instance (Succ a' a, PosTypeI b, Sub a' b c) => Sub a (Pos b) c
instance (NumTypeI a) => Sub a Zero a

class (Add a b c, Sub c b a) => Sum a b c | a b -> c, a c -> b, b c -> a
instance (Add a b c, Sub c b a) => Sum a b c

azz :: Add Zero Zero Zero => ()
azz = ()

bzz :: ()
bzz = azz

class (NumTypeI n) => PosTypeI n

type Scalar d = Quantity d Float

class Mul d d' d'' | d d' -> d''
instance (Sum l  l'  l'',
          Sum t  t'  t''
          ) => Mul (Dim l t)
                                 (Dim l'  t' )
                                 (Dim l'' t'')

data Dim l t
type DOne         = Dim Zero Zero
type DTime        = Dim Zero Pos1 
type DLength      = Dim Pos1 Zero

newtype Quantity d a = Dimensional a

(*~) :: P.Num a => a -> Quantity d a -> Quantity d a
x *~ Dimensional y = Dimensional (x P.* y)

one :: P.Num a => Quantity DOne a
one = Dimensional 1

second  :: P.Num a => Quantity DTime a
second  = Dimensional 1

zogo :: Scalar DOne
zogo = undefined

mul :: Mul a b c => Scalar a -> Scalar b -> Scalar c
mul = undefined 

a :: Scalar DTime
a = 2.5 *~ second

force :: Scalar DTime
force = undefined

pl :: Scalar DTime
pl = mul (1 *~ one) force

bug :: Scalar DLength
bug = (200 *~ one) `mul` zogo
