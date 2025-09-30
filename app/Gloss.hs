-- Gloss encapsulates the Graphics.Gloss specific parts of the code.
module Gloss (
  Color
  , black
  , white
  , getRGBA
  , rgba
  , rgb
  , anim
  , animFile

  , FromFloat
  , fromFloat
) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Graphics.Gloss as G
import qualified Codec.Picture as PIC

-- FromFloat is an abbreviated Fractional, because I dont want to make a full Fractional impl for Color.
-- This allows us to use `lerp` over Color.
class Num a => FromFloat a where
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

-- genImage returns an image for the picture library.
genImage :: (Int, Int) -> ColorAnim -> Float -> PIC.Image PIC.PixelRGB8
genImage (xsz, ysz) an ts = PIC.generateImage getPixel xsz ysz
  where
    getPixel x y = color2pixel (an ts (frac x xsz, frac y ysz))
    color2pixel c = let (r,g,b,_a) = getRGBA c in PIC.PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
    toWord8 x = fromIntegral (floor (x * 255) :: Int)
    frac :: Int -> Int -> Float
    frac x xs = (fromIntegral x) / (fromIntegral xs)

-- anim will show an animation defined by f in a window with (x,y) ranging over [0..1].
anim :: ColorAnim -> IO ()
anim f = do
  let wsz = (500, 500)
  let disp = G.InWindow "Animate!" wsz (10, 10)
  G.animate disp white (genRGBA wsz f)

-- animFile writes gif of animation to fn from time [0..endtime].
animFile :: FilePath -> Float -> ColorAnim -> IO ()
animFile fn endtime an = do
  let step = 0.1
  let dims = (200, 200)
  let gifDelay = floor (step * 100) -- 100ths of a second
  let images = [genImage dims an ts | ts <- [0,step..endtime]]
  case PIC.writeGifAnimation fn gifDelay PIC.LoopingForever images of
    (Left errmsg) -> print errmsg
    (Right res) -> res
