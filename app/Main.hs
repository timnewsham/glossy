{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import GraphLib
import Lib

main :: IO ()
main = display circle
-- main = display (\ts -> colorBitmap black white bmCircle)


-- returns a color for each (ts, x,y) with (x,y) normalized to 0..1 and ts in seconds.
blinkingGradient :: ColorAnim
-- blinkingGradient :: Float -> Float -> Float -> Color
blinkingGradient ts x y = lerp grad white (cosCycle 2 ts)
  where grad = makeColor x y 0 1

circle :: ColorAnim
-- circle :: Float -> Float -> Float -> Color
circle ts = maskImage (constImage black) (blinkingGradient ts) (bmCircle 0.75)

bmCircle :: Float -> Bitmap
bmCircle rad x y = r < rad
  where
    xx = lerp (-1.0) 1.0 x
    yy = lerp (-1.0) 1.0 y
    r = sqrt (xx*xx + yy*yy) :: Float
