module Lib (
  gradiantDiag
  , gradiantX

  , sinCycle
  , cosCycle
  , rampCycle
  , periodRad
  , scaleSin
  , clamp
  , lerp
  , unlerp
  , blend
  , blendAnim
  , fade

  , animOrigin
) where

import Data.Fixed (mod')

import Types

avg :: Fractional a => a -> a -> a
avg x y = (x+y)/2

-- gradientDiag returns a gradient from c0 at (0,0) to c1 at (1,1).
gradiantDiag :: Color -> Color -> ColorImage
gradiantDiag c0 c1 (x,y) = lerp (avg x y) c0 c1

-- gradiantX returns a gradiant in the x direction from c0 at 0 to c1 at 1.
-- Rotate this for gradiants in other directions.
gradiantX :: Color -> Color -> ColorImage
gradiantX c0 c1 (x, _y) = lerp x c0 c1

-- sinCyle cycles from [0..1] with period per using sin.
sinCycle :: Time -> Time -> Time
sinCycle per = scaleSin . sin . periodRad per

-- cosCyle cycles from [0..1] with period per using cos.
cosCycle :: Time -> Time -> Time
cosCycle per = scaleSin . cos . periodRad per

-- rampCycle cycles from [0..1] with period per using a linear ramp up.
rampCycle :: Time -> Time -> Time
rampCycle per x = mod' (x / per) 1.0

-- periodRad returns radians for sin/cos to cycle with period per.
periodRad :: Float -> Float -> Float
periodRad per x = 2 * pi * x / per

-- scaleSin scales the output of sin/cos to [0..1]
scaleSin :: Float -> Float
scaleSin x = 0.5 + 0.5 * x

-- clamp return x clamped between minx an maxx.
clamp :: Ord a => a -> a -> a -> a
clamp minx maxx x = if x < minx then minx else if x > maxx then maxx else x

-- lerp returns the linear interpolation between minv and maxv based on p, which is clamped to [0..1].
-- It works over things that support FromFloat, such as Floats and Colors.
lerp :: FromFloat a => Float -> a -> a -> a
lerp p minv maxv = (pmin * minv) + (pmax * maxv)
  where 
    p' = clamp 0 1 p
    pmin = fromFloat $ clamp 0 1 (1-p')
    pmax = fromFloat $ clamp 0 1 p'

-- unlerp scales v to [0..1] based on how far it is between minv and maxv.
-- It does not do any clamping and values outside [minv..maxv] will not be in [0..1].
unlerp :: Float -> Float -> Float -> Float
unlerp v minv maxv = (v - minv) / (maxv - minv)

-- blend from image1 to image2 by fade factor from [0..1].
blend :: FromFloat a => Float -> Image a -> Image a -> Image a
blend p img1 img2 = lerp p <$> img1 <*> img2

blendAnim :: FromFloat a => Float -> Anim a -> Anim a -> Anim a
blendAnim p an1 an2 = blend p <$> an1 <*> an2

-- fade from an1 to an2 over time using tfunc to calculate the fade factor from the time.
fade :: FromFloat a => (Float -> Float) -> Anim a -> Anim a -> Anim a
-- fade pfunc an1 an2 ts = blend (pfunc ts) (an1 ts) (an2 ts)
fade pfunc an1 an2 = blend <$> pfunc <*> an1 <*> an2

-- animOrigin shows the animation with the origin centered, with coordinates over [-1..1].
animOrigin :: ColorAnim -> IO ()
animOrigin = anim . fmap originImage
