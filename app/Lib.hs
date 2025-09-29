module Lib (
  circle -- TODO: move with bitmap defn
  , square

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

-- the unit circle
circle :: Bitmap
circle pos = mag pos < 1

-- the first quadrant square
square :: Bitmap
square (x, y) = (0 <= x && x < 1) && (0 <= y && y < 1)

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
-- XXX we want this to work from Floats over Colors.  so we need better types and the Float->Color conversion
lerp :: (Num a, FromFloat a) => Float -> a -> a -> a
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
blend :: (Num a, FromFloat a) => Float -> Image a -> Image a -> Image a
blend p img1 img2 = lerp p <$> img1 <*> img2

blendAnim :: (Num a, FromFloat a) => Float -> Anim a -> Anim a -> Anim a
blendAnim p an1 an2 = blend p <$> an1 <*> an2

-- fade from an1 to an2 over time using tfunc to calculate the fade factor from the time.
fade :: (Num a, FromFloat a) => (Float -> Float) -> Anim a -> Anim a -> Anim a
fade pfunc an1 an2 ts = blend (pfunc ts) (an1 ts) (an2 ts)
-- fade pfunc an1 an2 = blend <$> pfunc <*> an1 <*> an2

{-
-- withPos manipulates a (position-varying) image in a position-dependent way.
-- It takes a function from position to Image a, and generates an Image.
-- This plumbing can make it easier to get a handle on the position by
-- wrapping and unwrapping the Image function for you.
withPos :: (Coord -> Image a) -> Image a
withPos im = Image (\pos -> calcImage (im pos) pos)

-- withTime manipulates a (time-varying) animation in a time-dependent way.
-- It takes a function from time to an animation, and animates it.
-- This plumbing can make it easier to get a handle on the timestamp by
-- wrapping and unwrapping the Anim function for you.
withTime :: (Float -> Anim a) -> Anim a
withTime an = Anim (\ts -> unAnim (an ts) ts)

-- withTimePos manipulates a (time- and position-varying) animation in a time- and
-- position-varying way.
-- It takes a function from (time, coord) to an animation, and animates it.
-- This plumbing can make it easier to get a handle on the timestamp and position by
-- wrapping and unwrapping the Anim function for you.
withTimePos :: (Float -> Coord -> Anim a) -> Anim a
withTimePos an = Anim (\ts -> Image (\pos -> calcAnim (an ts pos) ts pos))
-- withTimePos an = Anim (\ts -> Image (\pos -> calcImage ((unAnim (an ts pos) ts)) pos))
-- withTimePos an = Anim (\ts -> withPos (\pos -> unAnim (an ts pos) ts))
-- withTimePos an = withTime (\ts -> constAnim (withPos (\pos -> unAnim (an ts pos) ts)))
-}

-- animOrigin shows the animation with the origin centered, with coordinates over [-1..1].
animOrigin :: ColorAnim -> IO ()
animOrigin = anim . fmap originImage
