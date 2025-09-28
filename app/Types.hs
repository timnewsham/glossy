-- Types for the images, bitmaps, and animations.
module Types (
  Image
  , mkImage
  , ColorImage
  , Bitmap
  , Anim
  , mkAnim
  , ColorAnim
  , colorBitmap
  , bwBitmap
  , constImage
  , transformImage
  , translateImage
  , scaleImage
  , originImage
  , rot
  , rotateImage
  , mixImage
  , maskImage
  , mapImage
  , mapImage2
  , mapImage3
  , constAnim
  , mapAnim
  , mapAnimTime
  , scaleAnim
  , translateAnim

  , animOrigin
) where

import Gloss

-- Image from Gloss
mkImage :: (Float -> Float -> a) -> Image a
mkImage = Image

-- A bitmap is an image of Bools.
-- It maps (x, y) to Bools.
type Bitmap = Image Bool

-- A ColorImage is an image of Colors.
-- It maps (x, y) to Colors.
type ColorImage = Image Color

-- Anim from Gloss
mkAnim :: (Float -> Float -> Float -> a) -> Anim a
mkAnim f = Anim (\ts -> Image (\x y -> f ts x y))

-- A ColorAnim is animation of Color images.
-- It maps timestamps in seconds and (x,y) to Colors.
type ColorAnim = Anim Color

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage v = Image (\_x _y -> v)

-- fmap over images value by value.
mapImage :: (a -> b) -> Image a -> Image b
mapImage f (Image imgf) = Image (\x y -> f (imgf x y))

-- combine two images with a function over values.
mapImage2 :: (a -> b -> c) -> Image a -> Image b -> Image c
mapImage2 f (Image img1f) (Image img2f) = Image (\x y -> f (img1f x y) (img2f x y))

-- combine three images with a function over values.
mapImage3 :: (a -> b -> c -> d) -> Image a -> Image b -> Image c -> Image d
mapImage3 f (Image img1f) (Image img2f) (Image img3f) = Image (\x y -> f (img1f x y) (img2f x y) (img3f x y))

-- transforms an image's coordinates by f.
-- note: the image will appear transformed by the inverse of f.
-- but for simplicity I'm not going to try to define inverse functions.
transformImage :: (Float -> Float -> (Float, Float)) -> Image a -> Image a
transformImage f (Image imgf) = Image (\x y -> let (x', y') = f x y in imgf x' y')

-- translate an image by (dx, dy).
translateImage :: Float -> Float -> Image a -> Image a
translateImage dx dy = transformImage (\x y -> (x-dx, y-dy))

-- scale an iamge by s.
-- not a Scale instance because Image a is not a distinct type.
scaleImage :: Float -> Image a -> Image a
scaleImage s = transformImage (\x y -> (x/s, y/s))

-- transform coordinates to display image at the origin.
originImage :: Image a -> Image a
originImage = transformImage (\x y -> (2*x-1, 2*y-1))

rot :: Float -> Float -> Float -> (Float, Float)
rot theta x y = (x * costheta - y * sintheta, x * sintheta + y * costheta)
  where
    costheta = cos theta
    sintheta = sin theta

rotateImage :: Float -> Image a -> Image a
rotateImage theta = transformImage $ rot (0-theta)

-- mixImage shows background image bg where bm is false and foreground image fg where bm is true.
mixImage :: Bitmap -> Image a -> Image a -> Image a
mixImage = mapImage3 (\bmv bgv fgv -> if bmv then fgv else bgv)

-- maskImage shows image fg where bm is true, and zero when bm is false.
maskImage :: Num a => Bitmap -> Image a -> Image a
maskImage = mapImage2 (\bmv fgv -> if bmv then fgv else 0)

-- colorBitmap returns a color image with bg color when bm is false and fg color when bm is true.
colorBitmap :: Color -> Color -> Bitmap -> ColorImage
colorBitmap bg fg bm = mixImage bm (constImage bg) (constImage fg)

-- bwBitmap converts a bitmap to a black and white color image.
bwBitmap :: Bitmap -> ColorImage
bwBitmap = colorBitmap black white

-- constAnim is an animation of a constant image.
constAnim :: Image a -> Anim a
constAnim img = Anim (\_ts -> img)

-- mapAnim maps f over the values in each animated image.
-- mapAnim runs f over each animated image.
mapAnim :: (Image a -> Image b) -> Anim a -> Anim b
mapAnim f (Anim an) = Anim (\ts -> f (an ts))

mapAnimTime :: (Float -> Float) -> Anim a -> Anim a
mapAnimTime f (Anim an) = Anim (\ts -> an (f ts))

-- scaleAnim speeds up the animation by s.
scaleAnim :: Float -> Anim a -> Anim a
scaleAnim s = mapAnimTime (*s)

-- translateAnim skips the animation backwards in time by dt.
translateAnim :: Float -> Anim a -> Anim a
translateAnim dt = mapAnimTime (\t -> t - dt)

-- animOrigin shows the animation with the origin centered, with coordinates over [-1..1].
animOrigin :: ColorAnim -> IO ()
animOrigin = anim . mapAnim originImage
