-- Gloss encapsulates graphics support from Graphics.Gloss.
module Gloss (
  Coord
  , Image(..)
  , calcImage
  , Anim(..)
  , unAnim
  , calcAnim
  , anim
  , scaleColor

  -- re-exports.
  , Color
  , makeColor
  , white
  , black
) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G
import Graphics.Gloss (Color, makeColor, white, black, rgbaOfColor)

type Coord = (Float, Float)

-- An image maps (x,y) (often in range [0..1]) to values.
data Image a = Image (Coord -> a)

calcImage :: Image a -> Coord -> a
calcImage (Image f) = f

-- An Anim maps timestamps in seconds to images.
-- It maps timestamps in seconds and (x,y) to values.
data Anim a = Anim (Float -> Image a)

unAnim :: Anim a -> Float -> Image a
unAnim (Anim f) ts = f ts

calcAnim :: Anim a -> Float -> Coord -> a
calcAnim (Anim f) ts coord = calcImage (f ts) coord

-- ColorAnim maps (ts, x, y) to a color.
type ColorAnim = Anim Color

-- scaleColor adjusts the RGB channels but leaves A unchanged.
scaleColor :: Float -> Color -> Color
scaleColor s c = makeColor (s*r) (s*g) (s*b) a
    where (r,g,b,a) = rgbaOfColor c

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
