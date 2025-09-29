{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Types
import Lib

main :: IO ()
main = do
  let _t0 = gradCircle 0.75
  let _t1 = rotCircle . periodRad 5
  let _t1b = rotCircle2 . periodRad 5
  let _t2 = throbCircle
  let _t3 = fmap (scaleImage 0.5) (gradCircle 0.75)
  let _t4 = fmap (transformImage (translate (0.3, 0) . scale 0.5)) (gradCircle 0.75)
  let _t5 = (fastForward (-10) . speedUp 0.5) (gradCircle 0.75)
  let _t6 = speedUp 5 (gradCircle 0.75)
  let _t7 = constAnim (bwBitmap (scaleImage 0.75 circle))
  let _t8 = constAnim (bwBitmap (bmChecker 8))
  let _t9 = blendAnim 0.5 _t8  _t1
  let _t9b = fade (cosCycle 3) _t8 _t1
  animOrigin _t9b

gradientFirstQuad :: ColorImage
gradientFirstQuad (x,y) = rgb x y 0

-- a gradient centered at the origin.
gradient :: ColorImage
gradient = unoriginImage gradientFirstQuad

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
blinkingGradient ts = blend (cosCycle 2 ts) gradient (constImage white)

-- a circle of radius r at the origin colored by a (unchanged by radius) blinking grandient.
gradCircle :: Float -> ColorAnim
gradCircle rad = maskAnim (scaleImage rad circle) blinkingGradient

-- maskAnim shows an where m is True.
maskAnim :: Num a => Bitmap -> Anim a -> Anim a
maskAnim m an = maskImage m . an

throbCircle :: ColorAnim
throbCircle ts = gradCircle rad ts
  where
    rad = 0.1 + 0.75 * (cosCycle 7 ts)

-- A gradient checkerboard circle rotated by theta.
rotCircle :: Float -> ColorImage
rotCircle theta = rotateImage theta (maskImage mask gradient)
  where mask = andBitmap (bmCircle 0.75) (bmChecker 8)

-- A gradient circle on a checkerboard, rotated by theta
rotCircle2 :: Float -> ColorImage
rotCircle2 theta = rotateImage theta (maskImage mask gradient)
  where mask = orBitmap (bmCircle 0.75) (bmChecker 8)

bmCircle rad = scaleImage rad circle
bmChecker n = unoriginImage $ checker n

-- an n x n checkerboard in the first quadrant.
checker :: Int -> Bitmap
checker n (x, y) = even (tileNum x + tileNum y)
  where
    -- tileNum returns the tile number for v where there are n tiles from [0..1].
    tileNum :: Float -> Int
    tileNum v = floor (v * fromIntegral n)
