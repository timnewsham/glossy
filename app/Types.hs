-- Types for the images, bitmaps, and animations.
module Types (
  Coord
  , Image(..)
  , mkImage
  , calcImage
  , ColorImage
  , Bitmap
  , Anim(..)
  , mkAnim
  , calcAnim
  , unAnim
  , ColorAnim
  , colorBitmap
  , bwBitmap
  , constImage
  , transformImage
  , translate
  , translateImage
  , scaleF
  , scaleImage
  , unitToOrigin
  , originImage
  , originToUnit
  , unoriginImage
  , rot
  , rotateImage
  , mixImage
  , maskImage
  , constAnim
  , mapImages
  , warpTime
  , speedUp
  , fastForward

  -- re-exports.
  , Color
  , makeColor
  , rgbaOfColor
  , white
  , black
) where

import Control.Applicative

import GlossTypes

type Coord = (Float, Float)

-- An image maps (x,y) (often in range [0..1]) to values.
data Image a = Image (Coord -> a)

mkImage :: (Coord -> a) -> Image a
mkImage = Image

calcImage :: Image a -> Coord -> a
calcImage (Image f) = f

-- A bitmap is an image of Bools.
-- It maps (x, y) to Bools.
type Bitmap = Image Bool

-- A ColorImage is an image of Colors.
-- It maps (x, y) to Colors.
type ColorImage = Image Color

-- An Anim maps timestamps in seconds to images.
-- It maps timestamps in seconds and (x,y) to values.
data Anim a = Anim (Float -> Image a)

unAnim :: Anim a -> Float -> Image a
unAnim (Anim f) ts = f ts

calcAnim :: Anim a -> Float -> Coord -> a
calcAnim (Anim f) ts coord = calcImage (f ts) coord

mkAnim :: (Float -> Coord -> a) -> Anim a
mkAnim f = Anim (\ts -> Image (\coord -> f ts coord))

-- A ColorAnim is animation of Color images.
-- It maps timestamps in seconds and (x,y) to Colors.
type ColorAnim = Anim Color

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage = Image . const

-- fmap over images value by value at each coordinate.
instance Functor Image where
  fmap f (Image imgf) = Image (f . imgf)

instance Applicative Image where
  pure v = mkImage (\_coord -> v)
  (Image imgf1) <*> (Image imgf2) = Image (\coord -> imgf1 coord (imgf2 coord))

-- combine two images with a function over values.
-- mapImage2 :: (a -> b -> c) -> Image a -> Image b -> Image c
-- mapImage2 f (Image img1f) (Image img2f) = Image (\coord -> f (img1f coord) (img2f coord))
-- mapImage2 = liftA2

-- combine three images with a function over values.
-- mapImage3 :: (a -> b -> c -> d) -> Image a -> Image b -> Image c -> Image d
-- mapImage3 f (Image img1f) (Image img2f) (Image img3f) = Image (\coord -> f (img1f coord) (img2f coord) (img3f coord))
-- mapImage3 = liftA3

-- transforms an image's coordinates by f.
-- note: the image will appear transformed by the inverse of f.
-- but for simplicity I'm not going to try to define inverse functions.
transformImage :: (Coord -> Coord) -> Image a -> Image a
transformImage f (Image imgf) = Image (imgf . f)

translate :: Coord -> Coord -> Coord
translate (dx, dy) (x,y) = (x + dx, y + dy)

-- translate an image by (dx, dy).
translateImage :: Coord -> Image a -> Image a
translateImage (dx, dy) = transformImage $ translate (-dx, -dy)

-- TODO: make Coord a Scale
scaleF :: Float -> Coord -> Coord
scaleF s (x, y) = (x*s, y*s)

-- scale an iamge by s.
-- not a Scale instance because Image a is not a distinct type.
-- XXX make Image a Scale.
scaleImage :: Float -> Image a -> Image a
scaleImage s = transformImage (scaleF (1/s))

unitToOrigin :: Coord -> Coord
unitToOrigin = translate (-1, -1) . scaleF 2

-- transform coordinates to display image at the origin from first quadrant coordinates.
originImage :: Image a -> Image a
originImage = transformImage unitToOrigin

originToUnit :: Coord -> Coord
originToUnit = scaleF 0.5 . translate (1, 1)

-- transform coordinates to display image in first quadrant from centered coordinates.
unoriginImage :: Image a -> Image a
unoriginImage = transformImage originToUnit

rot :: Float -> Coord -> Coord
rot theta (x, y) = (x * costheta - y * sintheta, x * sintheta + y * costheta)
  where
    costheta = cos theta
    sintheta = sin theta

rotateImage :: Float -> Image a -> Image a
rotateImage theta = transformImage $ rot (0-theta)

-- mixImage shows background image bg where bm is false and foreground image fg where bm is true.
mixImage :: Bitmap -> Image a -> Image a -> Image a
mixImage = liftA3 (\bmv bgv fgv -> if bmv then fgv else bgv)

-- maskImage shows image fg where bm is true, and zero when bm is false.
maskImage :: Num a => Bitmap -> Image a -> Image a
maskImage = liftA2 (\bmv fgv -> if bmv then fgv else 0)

-- colorBitmap returns a color image with bg color when bm is false and fg color when bm is true.
colorBitmap :: Color -> Color -> Bitmap -> ColorImage
colorBitmap bg fg bm = mixImage bm (constImage bg) (constImage fg)

-- bwBitmap converts a bitmap to a black and white color image.
bwBitmap :: Bitmap -> ColorImage
bwBitmap = colorBitmap black white

-- constAnim is an animation of a constant image.
constAnim :: Image a -> Anim a
constAnim img = Anim (\_ts -> img)

-- mapAnim runs f over each animated image.
-- fmap on an Anim will run f on each value from each image in Anim.
instance Functor Anim where
  -- at each ts we get the image and map f over it.
  -- Anim (\ts -> fmap f (an ts))
  fmap f (Anim an) = Anim (fmap f . an)

-- mapImages will run f over each image in an Anim.
-- at each ts we get the image and use f to map it.
--   Anim (\ts -> f (an ts))
mapImages :: (Image a -> Image b) -> Anim a -> Anim b
mapImages f (Anim an) = Anim (f . an)

-- warpTime uses (f time) as the time in the animation.
warpTime :: (Float -> Float) -> Anim a -> Anim a
warpTime f (Anim an) = Anim (\ts -> an (f ts))

-- speedUp speeds up the animation by s.
speedUp :: Float -> Anim a -> Anim a
speedUp s = warpTime (*s)

-- fastForward skips the animation forward by dt.
fastForward :: Float -> Anim a -> Anim a
fastForward dt = warpTime (+dt)
