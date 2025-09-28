{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Gloss
import Lib
import Types

main :: IO ()
main = display circle
-- main = display (\ts -> bwBitmap (bmCircle 0.75))

blinkingGradient :: ColorAnim
blinkingGradient ts x y = lerp grad white (cosCycle 2 ts)
  where grad = makeColor x y 0 1

circle :: ColorAnim
circle ts = maskImage (constImage black) (blinkingGradient ts) (bmCircle 0.75)

bmCircle :: Float -> Bitmap
bmCircle rad x y = r < rad
  where
    xx = lerp (-1.0) 1.0 x
    yy = lerp (-1.0) 1.0 y
    r = sqrt (xx*xx + yy*yy) :: Float
