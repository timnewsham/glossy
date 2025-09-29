-- Gloss encapsulates graphics support from Graphics.Gloss.
module Gloss (
  Color
  , black
  , white
  , getRGBA
  , rgba
  , rgb
  , anim

  , FromFloat
  , fromFloat
) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G

-- FromFloat is an abbreviated Rational because I dont want to make an impl for Color for it.
class FromFloat a where
  fromFloat :: Float -> a

instance FromFloat Float where
  fromFloat = id

-- Color comes straight from Graphics.Gloss.
-- It contains (r,g,b,a) as Floats.
-- It is a Num.
-- TODO: we could make our own color type, but I would rather not make a Num impl for it.
type Color = G.Color

instance FromFloat G.Color where
  fromFloat x = rgba x x x 1

black :: Color
black = G.black

white :: Color
white = G.white

-- getRGBA extracts the color components.
getRGBA :: Color -> (Float, Float, Float, Float)
getRGBA = G.rgbaOfColor

-- rgba makes a color from r,g,b,a values.
rgba :: Float -> Float -> Float -> Float -> Color
rgba = G.makeColor

-- rgb makes a color from r,g,b values.
rgb :: Float -> Float -> Float -> Color
rgb r g b = rgba r g b 1

type ColorAnim = Float -> (Float, Float) -> G.Color

-- genRGBA returns an RGBA bitmap with size (xsz,ysz) using an.
genRGBA :: (Int, Int) -> ColorAnim -> Float -> G.Picture
genRGBA (xsz, ysz) an ts = G.bitmapOfByteString xsz ysz fmt bs False
  where
    bs = B.pack $ concat $ [color2bytes (an ts (frac x xsz, frac y ysz)) | y <- [0..ysz-1], x <- [0..xsz-1]]
    toWord8 x = fromIntegral (floor (x * 255) :: Int)
    color2bytes :: Color -> [Word8]
    color2bytes c = let (r,g,b,a) = getRGBA c in map toWord8 [r,g,b,a]
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
