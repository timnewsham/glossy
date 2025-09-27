{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Data.Word (Word8)
import Data.Fixed (mod')
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G
import Graphics.Gloss (Color, makeColor, white, black)


-- returns a color for each (x,y,ts) with (x,y) normalized to 0..1 and ts in seconds.
blinkingGradient :: Float -> Float -> Float -> Color
blinkingGradient x y ts = makeColor x y (mod' (ts/2) 1.0) 1

getCirc :: Float -> Float -> Float -> Color
getCirc x y ts = if r < 0.75 then (blinkingGradient x y ts) else black
  where
    xx = lerp (-1.0) 1.0 x
    yy = lerp (-1.0) 1.0 y
    r = sqrt (xx*xx + yy*yy)

-- lerp returns the linear interpolation between minv and maxv based on t, which is clamped to [0..1].
-- TODO: generalize this. It should work for any scalable value for float t.
--lerp :: Scale a => a -> a -> Float -> a
lerp :: Float -> Float -> Float -> Float
lerp minv maxv t = scale minv (1-t') + scale maxv t'
  where t' = clamp 0.0 1.0 t

-- clamp return x clamped between minx an maxx.
clamp :: Ord a => a -> a -> a -> a
clamp minx maxx x = if x < minx then minx else if x > maxx then maxx else x

class Scale a where
  scale :: Float -> a -> a

instance Scale Float where
  scale s x = s * x

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
  --let f = getColor
  let f = getCirc
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)
