{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Gloss
import Lib
import Types

main :: IO ()
-- main = animOrigin circle075
main = animOrigin throbCircle
-- main = animOrigin (mapAnim (scaleImage 0.5) circle075)
-- main = animOrigin (mapAnim (translateImage 0.3 0 . scaleImage 0.5) circle075)
-- main = animOrigin (translateAnim 10 (scaleAnim 0.5 circle075))
-- main = animOrigin (scaleAnim 0.5 circle075)
-- main = animOrigin (constAnim (bwBitmap (bmCircle 0.75)))

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
blinkingGradient ts x y = lerp grad white (cosCycle 2 ts)
  where
    grad = makeColor x' y' 0 1
    x' = unlerp (-1.0) 1.0 x
    y' = unlerp (-1.0) 1.0 y

-- a circle at the origin colored by a blinking grandient.
circle :: Float -> ColorAnim
circle rad ts = maskImage (constImage black) (blinkingGradient ts) (bmCircle rad)

throbCircle :: ColorAnim
throbCircle ts = circle (0.1 + 0.75 * (cosCycle 7 ts)) ts

circle075 :: ColorAnim
circle075 = circle 0.75

-- a circle at the origin with radius rad.
bmCircle :: Float -> Bitmap
bmCircle rad x y = d < rad
  where d = sqrt (x*x + y*y) :: Float
