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
  , maskImage
  , mapImage
  , mapImage2
  , mapImage3
) where

import Graphics.Gloss

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

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage v _ _ = v

mapImage :: (a -> b) -> Image a -> Image b
mapImage f img x y = f (img x y)

mapImage2 :: (a -> b -> c) -> Image a -> Image b -> Image c
mapImage2 f img1 img2 x y = f (img1 x y) (img2 x y)

mapImage3 :: (a -> b -> c -> d) -> Image a -> Image b -> Image c -> Image d
mapImage3 f img1 img2 img3 x y = f (img1 x y) (img2 x y) (img3 x y)

-- maskImage shows background image bg where bm is false and foreground image fg where bm is true.
maskImage :: Image a -> Image a -> Bitmap -> Image a
maskImage = mapImage3 (\bgv fgv bmv -> if bmv then fgv else bgv)

-- colorBitmap returns a color image with bg color when bm is false and fg color when bm is true.
colorBitmap :: Color -> Color -> Bitmap -> ColorImage
colorBitmap bg fg bm = maskImage (constImage bg) (constImage fg) bm

bwBitmap :: Bitmap -> ColorImage
bwBitmap = colorBitmap black white
