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
  let _t4 = fmap (\img -> img . (scale 2 . translate (0-0.3, 0))) (gradCircle 0.75)
  let _t5 = (fastForward (-10) . speedUp 0.5) (gradCircle 0.75)
  let _t6 = speedUp 5 (gradCircle 0.75)
  let _t7 = const (bwBitmap (scaleImage 0.75 circle))
  let _t8 = const (bwBitmap (checker 8))
  let _t9 = blendAnim 0.5 _t8  _t1
  let _t9b = fade (cosCycle 3) _t8 _t1
  let _t10 = const $ unoriginImage $ gradiantX red green
  let _t10b = const $ unoriginImage $ gradiantDiag red green
  let _t11 = const $ twist (1/8) (bwBitmap $ checker 8)
  let _t12 = twistedCircle
  -- animFileOrigin "twistedCircle.gif" 3 twistedCircle
  animOrigin _t12

red = rgb 1 0 0
green = rgb 0 1 0

-- twist twists an image into a spiral with nrot rotations at unit distance from the origin.
twist :: Float -> Image a -> Image a
twist nrot im pos = rotateImage (2 * pi * nrot * mag pos) im pos

twistedCircle :: ColorAnim
twistedCircle ts = twist nrot checkeredCircle
  where
    nrot = 0.5 * (sin . periodRad 3) ts
    mask = andBitmap (circleRad 1) (checker 8)
    checkeredCircle = maskImage mask gradient

gradientFirstQuad :: ColorImage
gradientFirstQuad (x,y) = rgb x y 0

-- a gradient centered at the origin.
gradient :: ColorImage
gradient = unoriginImage gradientFirstQuad

-- a gradient centered at the origin that cycles with time.
blinkingGradient :: ColorAnim
blinkingGradient ts = blend (cosCycle 2 ts) gradient (const white)

-- a circle of radius r at the origin colored by a (unchanged by radius) blinking grandient.
gradCircle :: Float -> ColorAnim
gradCircle rad = maskAnim (scaleImage rad circle) blinkingGradient

throbCircle :: ColorAnim
throbCircle ts = gradCircle rad ts
  where
    rad = 0.1 + 0.75 * (cosCycle 7 ts)

-- A gradient checkerboard circle rotated by theta.
rotCircle :: Float -> ColorImage
rotCircle theta = rotateImage theta (maskImage mask gradient)
  where mask = andBitmap (circleRad 0.75) (checker 8)

-- A gradient circle on a checkerboard, rotated by theta
rotCircle2 :: Float -> ColorImage
rotCircle2 theta = rotateImage theta (maskImage mask gradient)
  where mask = orBitmap (circleRad 0.75) (checker 8)

circleRad rad = scaleImage rad circle
checker n = unoriginImage $ checkerboard n
