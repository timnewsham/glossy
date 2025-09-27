{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Data.Word (Word8)
import Data.Fixed (mod')
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G
import Graphics.Gloss (Color, makeColor, white, black)


-- returns a color for each (x,y,ts) with (x,y) normalized to 0..1 and ts in seconds.
blinkingGradient :: Float -> Float -> Float -> Color
--blinkingGradient x y ts = makeColor x y (mod' (ts/2) 1.0) 1
blinkingGradient x y ts = lerp grad white blink
  where
    grad = makeColor x y 0 1
    blink = mod' (ts/2) 1.0

circle :: Float -> Float -> Float -> Color
circle x y ts = if r < 0.75 then (blinkingGradient x y ts) else black
  where
    xx = lerp (-1.0) 1.0 x
    yy = lerp (-1.0) 1.0 y
    r = sqrt (xx*xx + yy*yy) :: Float

-- clamp return x clamped between minx an maxx.
clamp :: Ord a => a -> a -> a -> a
clamp minx maxx x = if x < minx then minx else if x > maxx then maxx else x

-- lerp returns the linear interpolation between minv and maxv based on t, which is clamped to [0..1].
-- You can lerp anything that can be scaled and added.
lerp :: (Scale a, Num a) => a -> a -> Float -> a
lerp minv maxv t = scale (1-t') minv + scale t' maxv
  where t' = clamp 0.0 1.0 t

-- Scale a is a type that can be scaled by a float.
class Scale a where
  scale :: Float -> a -> a

instance Scale Float where
  scale s x = s * x

instance Scale Int where
  scale s x = floor (s * fromIntegral x)

-- Scaling a color adjusts the RGB channels but leaves A unchanged.
instance Scale Color where
  scale s c = makeColor (s*r) (s*g) (s*b) a
    where (r,g,b,a) = G.rgbaOfColor c

-- genRGBA returns an RGBA bitmap with size (xsz,ysz) using picfunc.
genRGBA :: (Int, Int) -> (Float -> Float -> Float -> Color) -> Float -> G.Picture
genRGBA (xsz, ysz) picfunc ts = G.bitmapOfByteString xsz ysz fmt bs False
  where
    bs = B.pack $ concat $ [color2bytes (picfunc (frac x xsz) (frac y ysz) ts) | y <- [0..ysz-1], x <- [0..xsz-1]]
    clampToByte x = fromIntegral (floor (x * 255) :: Int)
    color2bytes :: Color -> [Word8]
    color2bytes c = let (r,g,b,a) = G.rgbaOfColor c in map clampToByte [r,g,b,a]
    frac :: Int -> Int -> Float
    frac x xs = (fromIntegral x) / (fromIntegral xs)
    fmt :: G.BitmapFormat
    fmt = G.BitmapFormat G.BottomToTop G.PxRGBA

main :: IO ()
main = do
  --let f = blinkingGradient
  let f = circle
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)
