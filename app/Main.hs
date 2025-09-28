{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Types
import Lib

main :: IO ()
main = do
  let _t0 = circle 0.75
  let _t1 = Anim (rotCircle . periodRad 5)
  let _t1b = Anim (rotCircle2 . periodRad 5)
  let _t2 = throbCircle
  let _t3 = mapImages (scaleImage 0.5) (circle 0.75)
  let _t4 = mapImages (transformImage (translate (0.3, 0) . scaleF 0.5)) (circle 0.75)
  let _t5 = (fastForward (-10) . speedUp 0.5) (circle 0.75)
  let _t6 = speedUp 5 (circle 0.75)
  let _t7 = constAnim (bwBitmap (bmCircle 0.75))
  let _t8 = constAnim (bwBitmap (bmChecker 8))
  let _t9 = lerp 0.5 _t8 _t1
  let _t9b = fade (cosCycle 3) _t8 _t1
  animOrigin _t9b

gradientFirstQuad :: ColorImage
gradientFirstQuad = mkImage (\(x,y) -> makeColor x y 0 1)

-- a gradient centered at the origin.
gradient :: ColorImage
gradient = unoriginImage gradientFirstQuad

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
-- blinkingGradient = fade (cosCycle 2) (constAnim gradient) (constAnim $ constImage white)
-- blinkingGradient = withTime (\ts -> constAnim (blend (cosCycle 2 ts) gradient (constImage white)))
blinkingGradient = Anim (\ts -> blend (cosCycle 2 ts) gradient (constImage white))

-- a circle at the origin colored by a blinking grandient.
circle :: Float -> ColorAnim
circle rad = maskAnim (bmCircle rad) blinkingGradient

-- maskAnim shows an where m is True.
maskAnim :: Num a => Bitmap -> Anim a -> Anim a
maskAnim m an = Anim (maskImage m . unAnim an)

throbCircle :: ColorAnim
throbCircle = withTime (circle . rad)
  where
    rad ts = 0.1 + 0.75 * (cosCycle 7 ts)

-- A gradient checkerboard circle rotated by theta.
rotCircle :: Float -> ColorImage
rotCircle theta = rotateImage theta (maskImage mask gradient)
  where mask = mulBitmap (bmCircle 0.75) (bmChecker 8)

-- A gradient circle on a checkerboard, rotated by theta
rotCircle2 :: Float -> ColorImage
rotCircle2 theta = rotateImage theta (maskImage mask gradient)
  where mask = sumBitmap (bmCircle 0.75) (bmChecker 8)

-- a circle at the origin with radius rad.
bmCircle :: Float -> Bitmap
bmCircle rad = mkImage (\pos -> mag pos < rad)

-- an n x n checkerboard at the origin from [-1..1].
bmChecker :: Int -> Bitmap
bmChecker n = unoriginImage $ mkImage (\(x,y) -> even (tileNum n x + tileNum n y))

-- tileNum returns the tile number for v where there are n tiles from [0..1].
tileNum :: Int -> Float -> Int
tileNum n v = floor (v * fromIntegral n)
