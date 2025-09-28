-- Types for the images, bitmaps, and animations.
module Types (
  Image
  , ColorImage
  , Bitmap
  , Anim
  , ColorAnim
  , colorBitmap
  , bwBitmap
  , constImage
  , translateImage
  , scaleImage
  , maskImage
  , mapImage
  , mapImage2
  , mapImage3
  , constAnim
  , mapAnim
  , mapAnimTime
  , scaleAnim
  , translateAnim
) where

import Graphics.Gloss

-- An image maps (x,y) (often in range [0..1]) to values.
-- Note: we're not defining new types or data types, to keep it simple,
-- at the expense of not being able to use type classes in places.
type Image a = Float -> Float -> a

-- A bitmap is an image of Bools.
-- It maps (x, y) to Bools.
type Bitmap = Image Bool

-- A ColorImage is an image of Colors.
-- It maps (x, y) to Colors.
type ColorImage = Image Color

-- An Anim maps timestamps in seconds to images.
-- It maps timestamps in seconds and (x,y) to values.
type Anim a = Float -> Image a

-- A ColorAnim is animation of Color images.
-- It maps timestamps in seconds and (x,y) to Colors.
type ColorAnim = Anim Color

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage v _ _ = v

-- fmap over images value by value.
mapImage :: (a -> b) -> Image a -> Image b
mapImage f img x y = f (img x y)

-- combine two images with a function over values.
mapImage2 :: (a -> b -> c) -> Image a -> Image b -> Image c
mapImage2 f img1 img2 x y = f (img1 x y) (img2 x y)

-- combine three images with a function over values.
mapImage3 :: (a -> b -> c -> d) -> Image a -> Image b -> Image c -> Image d
mapImage3 f img1 img2 img3 x y = f (img1 x y) (img2 x y) (img3 x y)

-- translate an image by (dx, dy).
translateImage :: Float -> Float -> Image a -> Image a
translateImage dx dy img x y = img (x - dx) (y - dy)

-- scale an iamge by s.
-- not a Scale instance because Image a is not a distinct type.
scaleImage :: Float -> Image a -> Image a
scaleImage s img x y = img (x / s) (y / s)

-- maskImage shows background image bg where bm is false and foreground image fg where bm is true.
maskImage :: Image a -> Image a -> Bitmap -> Image a
maskImage = mapImage3 (\bgv fgv bmv -> if bmv then fgv else bgv)

-- colorBitmap returns a color image with bg color when bm is false and fg color when bm is true.
colorBitmap :: Color -> Color -> Bitmap -> ColorImage
colorBitmap bg fg bm = maskImage (constImage bg) (constImage fg) bm

-- bwBitmap converts a bitmap to a black and white color image.
bwBitmap :: Bitmap -> ColorImage
bwBitmap = colorBitmap black white

-- constAnim is an animation of a constant image.
constAnim :: Image a -> Anim a
constAnim img _ = img

-- mapAnim maps f over the values in each animated image.
-- mapAnim runs f over each animated image.
mapAnim :: (Image a -> Image b) -> Anim a -> Anim b
mapAnim f anim ts = f (anim ts)

mapAnimTime :: (Float -> Float) -> Anim a -> Anim a
mapAnimTime f anim ts = anim (f ts)

-- scaleAnim speeds up the animation by s.
scaleAnim :: Float -> Anim a -> Anim a
scaleAnim s anim = mapAnimTime (*s) anim

-- translateAnim skips the animation backwards in time by dt.
translateAnim :: Float -> Anim a -> Anim a
translateAnim dt anim = mapAnimTime (\t -> t - dt) anim
