{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import GraphLib
import Lib

main :: IO ()
main = display circle


-- returns a color for each (ts, x,y) with (x,y) normalized to 0..1 and ts in seconds.
blinkingGradient :: ColorAnim
-- blinkingGradient :: Float -> Float -> Float -> Color
blinkingGradient ts x y = lerp grad white (cosCycle 2 ts)
  where grad = makeColor x y 0 1

circle :: ColorAnim
-- circle :: Float -> Float -> Float -> Color
circle ts x y = if r < 0.75 then (blinkingGradient ts x y) else black
  where
    xx = lerp (-1.0) 1.0 x
    yy = lerp (-1.0) 1.0 y
    r = sqrt (xx*xx + yy*yy) :: Float
