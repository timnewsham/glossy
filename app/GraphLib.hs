module GraphLib (
  display
  , Image
  , ColorImage
  , Bitmap
  , Anim
  , ColorAnim

  -- re-exports.
  , Color
  , makeColor
  , white
  , black
) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G
import Graphics.Gloss (Color, makeColor, white, black)

-- An image maps (x,y) in range [0..1] to values.
type Image a = Float -> Float -> a

-- A bitmap is an image of Bools.
-- It maps (x, y) in range [0..1] to Bools.
type Bitmap = Image Bool

-- A ColorImage is an image of Colors.
-- It maps (x, y) in range [0..1] to Colors.
type ColorImage = Image Color

-- An Anim maps timestamps in seconds to images.
-- It maps timestamps in seconds and (x,y) in range [0..1] to values.
type Anim a = Float -> Image a

-- A ColorAnim is animation of Color images.
-- It maps timestamps in seconds and (x,y) in range [0..1] to Colors.
type ColorAnim = Anim Color

-- genRGBA returns an RGBA bitmap with size (xsz,ysz) using picfunc.
genRGBA :: (Int, Int) -> ColorAnim -> Float -> G.Picture
genRGBA (xsz, ysz) picfunc ts = G.bitmapOfByteString xsz ysz fmt bs False
  where
    bs = B.pack $ concat $ [color2bytes (picfunc ts (frac x xsz) (frac y ysz)) | y <- [0..ysz-1], x <- [0..xsz-1]]
    clampToByte x = fromIntegral (floor (x * 255) :: Int)
    color2bytes :: Color -> [Word8]
    color2bytes c = let (r,g,b,a) = G.rgbaOfColor c in map clampToByte [r,g,b,a]
    frac :: Int -> Int -> Float
    frac x xs = (fromIntegral x) / (fromIntegral xs)
    fmt :: G.BitmapFormat
    fmt = G.BitmapFormat G.BottomToTop G.PxRGBA

-- display will show an animation defined by f in a window
display :: ColorAnim -> IO ()
display f = do
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)
