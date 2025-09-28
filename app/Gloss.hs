-- Gloss encapsulates graphics support from Graphics.Gloss.
module Gloss (
  anim
) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G

import Types

-- genRGBA returns an RGBA bitmap with size (xsz,ysz) using an.
genRGBA :: (Int, Int) -> ColorAnim -> Float -> G.Picture
genRGBA (xsz, ysz) an ts = G.bitmapOfByteString xsz ysz fmt bs False
  where
    bs = B.pack $ concat $ [color2bytes (calcAnim an ts (frac x xsz, frac y ysz)) | y <- [0..ysz-1], x <- [0..xsz-1]]
    clampToByte x = fromIntegral (floor (x * 255) :: Int)
    color2bytes :: Color -> [Word8]
    color2bytes c = let (r,g,b,a) = G.rgbaOfColor c in map clampToByte [r,g,b,a]
    frac :: Int -> Int -> Float
    frac x xs = (fromIntegral x) / (fromIntegral xs)
    fmt :: G.BitmapFormat
    fmt = G.BitmapFormat G.BottomToTop G.PxRGBA

-- anim will show an animation defined by f in a window with (x,y) ranging over [0..1].
anim :: ColorAnim -> IO ()
anim f = do
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)
