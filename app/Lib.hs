module Lib (
  sinCycle
  , cosCycle
  , rampCycle
  , periodRad
  , scaleSin
  , clamp
  , lerp
  , unlerp
  , Lerpable
  , blend
  , fade
  , withPos
  , withTime
  , withTimePos

  , animOrigin
) where

import Data.Fixed (mod')
import Control.Applicative

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
scaleSin x = unlerp x (-1.0) 1.0

-- clamp return x clamped between minx an maxx.
clamp :: Ord a => a -> a -> a -> a
clamp minx maxx x = if x < minx then minx else if x > maxx then maxx else x

-- lerp returns the linear interpolation between minv and maxv based on t, which is clamped to [0..1].
-- You can lerp anything that can be scaled and added.
lerp :: Lerpable a => Float -> a -> a -> a
lerp t minv maxv = ((1-t') ^* minv) ^+ (t' ^* maxv)
  where t' = clamp 0.0 1.0 t

-- unlerp scales v to [0..1] based on how far it is between minv and maxv.
-- It does not do any clamping and values outside [minv..maxv] will not be in [0..1].
unlerp :: Float -> Float -> Float -> Float
unlerp v minv maxv = (v - minv) / (maxv - minv)

-- blend from image1 to image2 by fade factor from [0..1].
blend :: Lerpable a => Float -> Image a -> Image a -> Image a
blend t img1 img2 = lerp t img1 img2

-- fade from an1 to an2 over time using tfunc to calculate the fade factor from the time.
fade :: Lerpable a => (Float -> Float) -> Anim a -> Anim a -> Anim a
fade tfunc an1 an2 = Anim (\ts -> blend (tfunc ts) (unAnim an1 ts) (unAnim an2 ts))

-- Lerpable a is a type that can be scaled by a float and added together.
class Lerpable a where
  scale :: Float -> a -> a
  add :: a -> a -> a

-- Just another name for scale.
(^*) :: Lerpable a => Float -> a -> a
(^*) = scale

-- Another name for add
(^+) :: Lerpable a => a -> a -> a
(^+) = add

-- You can scale floats.
instance Lerpable Float where
  scale s x = s * x
  add = (+)

-- You can scale ints.
instance Lerpable Int where
  scale s x = floor (s * fromIntegral x)
  add = (+)

-- Scaling a color adjusts the RGB channels but leaves A unchanged.
instance Lerpable Color where
  scale s c = makeColor (s*r) (s*g) (s*b) a
    where (r,g,b,a) = rgbaOfColor c
  add = (+)

-- XXX generalize to all applicatives
instance Lerpable a => Lerpable (Image a) where
  scale s img = fmap (scale s) img
  add = liftA2 add

instance Lerpable a => Lerpable (Anim a) where
  scale s img = fmap (scale s) img
  add = liftA2 add

-- withPos takes a function from position to Image a, and generates an Image.
-- This plumbing can make it easier to get a handle on the position by
-- wrapping and unwrapping the Image function for you.
withPos :: (Coord -> Image a) -> Image a
withPos im = Image (\pos -> calcImage (im pos) pos)

-- withTime takes a function from time to an animation, and animates it.
-- This plumbing can make it easier to get a handle on the timestamp by
-- wrapping and unwrapping the Anim function for you.
withTime :: (Float -> Anim a) -> Anim a
withTime an = Anim (\ts -> unAnim (an ts) ts)

-- withTime takes a function from (time, coord) to an animation, and animates it.
-- This plumbing can make it easier to get a handle on the timestamp and position by
-- wrapping and unwrapping the Anim function for you.
withTimePos :: (Float -> Coord -> Anim a) -> Anim a
-- XXX rewrite this more cleanly in terms of withTime and withPos.
withTimePos an = Anim (\ts -> withPos (\pos -> unAnim (an ts pos) ts))

-- animOrigin shows the animation with the origin centered, with coordinates over [-1..1].
animOrigin :: ColorAnim -> IO ()
animOrigin = anim . mapImages originImage
