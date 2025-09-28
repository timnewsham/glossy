module Lib (
  sinCycle
  , cosCycle
  , rampCycle
  , periodRad
  , scaleSin
  , clamp
  , lerp
  , unlerp

  , animOrigin
) where

import Data.Fixed (mod')
import Gloss (anim)
import Types

-- sinCyle cycles from [0..1] with period per using sin.
sinCycle :: Float -> Float -> Float
sinCycle per = scaleSin . sin . periodRad per

-- cosCyle cycles from [0..1] with period per using cos.
cosCycle :: Float -> Float -> Float
cosCycle per = scaleSin . cos . periodRad per

-- rampCycle cycles from [0..1] with period per using a linear ramp up.
rampCycle :: Float -> Float -> Float
rampCycle per x = mod' (x / per) 1.0

-- periodRad returns radians for sin/cos to cycle with period per.
periodRad :: Float -> Float -> Float
periodRad per x = 2 * pi * x / per

-- scaleSin scales the output of sin/cos to [0..1]
scaleSin :: Float -> Float
scaleSin = unlerp (-1.0) 1.0

-- clamp return x clamped between minx an maxx.
clamp :: Ord a => a -> a -> a -> a
clamp minx maxx x = if x < minx then minx else if x > maxx then maxx else x

-- lerp returns the linear interpolation between minv and maxv based on t, which is clamped to [0..1].
-- You can lerp anything that can be scaled and added.
lerp :: (Scale a, Num a) => a -> a -> Float -> a
lerp minv maxv t = (1-t') ^* minv + t' ^* maxv
  where t' = clamp 0.0 1.0 t

-- unlerp scales v to [0..1] based on how far it is between minv and maxv.
-- It does not do any clamping and values outside [minv..maxv] will not be in [0..1].
unlerp :: Float -> Float -> Float -> Float
unlerp minv maxv v = (v - minv) / (maxv - minv)

-- Scale a is a type that can be scaled by a float.
class Scale a where
  scale :: Float -> a -> a

-- Just another name for scale.
(^*) :: Scale a => Float -> a -> a
(^*) = scale

-- You can scale floats.
instance Scale Float where
  scale s x = s * x

-- You can scale ints.
instance Scale Int where
  scale s x = floor (s * fromIntegral x)

-- Scaling a color adjusts the RGB channels but leaves A unchanged.
instance Scale Color where
  scale s c = makeColor (s*r) (s*g) (s*b) a
    where (r,g,b,a) = rgbaOfColor c

-- animOrigin shows the animation with the origin centered, with coordinates over [-1..1].
animOrigin :: ColorAnim -> IO ()
animOrigin = anim . mapImages originImage
