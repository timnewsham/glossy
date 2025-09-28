{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Types
import Lib

main :: IO ()
main = do
  let _t0 = circle075
  let _t1 = (Anim (\ts -> (rotCircle (periodRad 5 ts))))
  let _t1b = (Anim (\ts -> (rotCircle2 (periodRad 5 ts))))
  let _t2 = throbCircle
  let _t3 = (mapImages (scaleImage 0.5) circle075)
  let _t4 = (mapImages (translateImage (0.3, 0) . scaleImage 0.5) circle075)
  let _t5 = (fastForward (-10) (speedUp 0.5 circle075))
  let _t6 = (speedUp 5 circle075)
  let _t7 = (constAnim (bwBitmap (bmCircle 0.75)))
  let _t8 = (constAnim (bwBitmap (bmChecker 8)))
  animOrigin _t1

-- a gradient centered at the origin.
gradient :: ColorImage
gradient = unoriginImage $ mkImage gradient'

gradient' (x, y) = makeColor x y 0 1

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
blinkingGradient = mkAnim blinkingGradient'

centeredToUnit :: Coord -> Coord
centeredToUnit (x,y) = (0.5 * x + 0.5, 0.5 * y + 0.5)

-- TODO: make lerp work over images and anims?
blinkingGradient' ts coord = lerp (calcImage gradient coord) white (cosCycle 2 ts)

-- a circle at the origin colored by a blinking grandient.
circle :: Float -> ColorAnim
circle rad = Anim (\ts -> maskImage (bmCircle rad) (unAnim blinkingGradient ts))

throbCircle :: ColorAnim
throbCircle = Anim throbCircle'

throbCircle' ts = unAnim (circle theta) ts
  where theta = 0.1 + 0.75 * (cosCycle 7 ts)

circle075 :: ColorAnim
circle075 = circle 0.75

-- A gradient circle rotated by theta.
rotCircle :: Float -> ColorImage
rotCircle theta = rotateImage theta (maskImage mask gradient)
  where mask = mulBitmap (bmCircle 0.75) (bmChecker 8)

rotCircle2 :: Float -> ColorImage
rotCircle2 theta = rotateImage theta (maskImage mask gradient)
  where mask = sumBitmap (bmCircle 0.75) (bmChecker 8)

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
