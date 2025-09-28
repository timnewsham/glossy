{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Gloss
import Lib
import Types

main :: IO ()
-- main = animOrigin circle075
main = animOrigin (Anim (\ts -> (rotCircle (periodRad 5 ts))))
-- main = animOrigin throbCircle
-- main = animOrigin (mapAnim (scaleImage 0.5) circle075)
-- main = animOrigin (mapAnim (translateImage 0.3 0 . scaleImage 0.5) circle075)
-- main = animOrigin (translateAnim 10 (scaleAnim 0.5 circle075))
-- main = animOrigin (scaleAnim 0.5 circle075)
-- main = animOrigin (constAnim (bwBitmap (bmCircle 0.75)))
-- main = animOrigin (constAnim (bwBitmap (bmChecker 8)))

-- a gradient circled at the origin.
gradient :: ColorImage
gradient = mkImage gradient'

gradient' (x, y) = makeColor x' y' 0 1
  where
    x' = unlerp (-1.0) 1.0 x
    y' = unlerp (-1.0) 1.0 y

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
blinkingGradient = mkAnim blinkingGradient'

centeredToUnit :: Coord -> Coord
centeredToUnit (x,y) = (x', y')
  where
    x' = unlerp (-1.0) 1.0 x
    y' = unlerp (-1.0) 1.0 y

blinkingGradient' ts coord = lerp grad white (cosCycle 2 ts)
  where
    (x', y') = centeredToUnit coord
    grad = makeColor x' y' 0 1

-- a circle at the origin colored by a blinking grandient.
circle :: Float -> ColorAnim
circle rad = Anim $ circle' rad

circle' rad ts = maskImage (bmCircle rad) (unAnim blinkingGradient ts)

throbCircle :: ColorAnim
throbCircle = Anim throbCircle'

throbCircle' ts = unAnim (circle theta) ts
  where theta = 0.1 + 0.75 * (cosCycle 7 ts)

circle075 :: ColorAnim
circle075 = circle 0.75

-- A gradient circle rotated by theta.
rotCircle :: Float -> ColorImage
rotCircle theta = rotateImage theta (mask gradient)
  where
    -- mask = maskImage (bmCircle 0.75) . maskImage (bmChecker 8)
    mask = maskImage (mapImage2 (&&) (bmCircle 0.75) (bmChecker 8))

-- a circle at the origin with radius rad.
bmCircle :: Float -> Bitmap
bmCircle rad = mkImage $ bmCircle' rad

bmCircle' rad (x, y) = d < rad
  where d = sqrt (x*x + y*y) :: Float

-- an nxn checkerboard at the origin from [-1..1].
bmChecker :: Int -> Bitmap
bmChecker n = mkImage $ bmChecker' n

bmChecker' n (x, y) = even (square x' + square y')
  where
    square v = floor (v * fromIntegral n) :: Int
    x' = unlerp (-1.0) 1.0 x
    y' = unlerp (-1.0) 1.0 y
