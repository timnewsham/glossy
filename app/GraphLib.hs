module GraphLib (
  display
  , Anim

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

type Anim = Float -> Float -> Float -> Color

-- genRGBA returns an RGBA bitmap with size (xsz,ysz) using picfunc.
genRGBA :: (Int, Int) -> Anim -> Float -> G.Picture
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

-- display will show an animation defined by f in a window
display :: Anim -> IO ()
display f = do
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)
