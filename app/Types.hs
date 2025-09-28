-- Types for the images, bitmaps, and animations.
module Types (
  Coord
  , mag
  , ang
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
  , mappendOver
  , mconcatOver
  , mulBitmap
  , mulBitmaps
  , sumBitmap
  , sumBitmaps
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
import Data.Monoid

import GlossTypes

type Coord = (Float, Float)

mag :: Coord -> Float
mag (x, y) = sqrt (x*x + y*y)

ang :: Coord -> Float
ang (x, y) = atan2 y x

translate :: Coord -> Coord -> Coord
translate (dx, dy) (x,y) = (x + dx, y + dy)

-- TODO: make Coord a Scale?
scaleF :: Float -> Coord -> Coord
scaleF s (x, y) = (x*s, y*s)

unitToOrigin :: Coord -> Coord
unitToOrigin = translate (-1, -1) . scaleF 2

originToUnit :: Coord -> Coord
originToUnit = scaleF 0.5 . translate (1, 1)

rot :: Float -> Coord -> Coord
rot theta (x, y) = (x * costheta - y * sintheta, x * sintheta + y * costheta)
  where
    costheta = cos theta
    sintheta = sin theta

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

-- fmap over images value by value at each coordinate.
instance Functor Image where
  fmap f (Image imgf) = Image (f . imgf)

instance Applicative Image where
  pure v = mkImage (\_coord -> v)
  (Image imgf1) <*> (Image imgf2) = Image (\coord -> imgf1 coord (imgf2 coord))

instance Semigroup a => Semigroup (Image a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Image a) where
  mempty = constImage mempty

-- XXX TODO: is there something like this in std lib already?
mconcatOver :: (Functor f, Monoid (f a1)) => (a1 -> b) -> (a2 -> a1) -> [f a2] -> f b
-- mconcatOver :: Monoid b => (b -> a) -> (a -> b) -> [Image a] -> Image a
-- convert [Image a] to [Image b] with inj where b is a Monoid
-- collapse to Image b with mconcat
-- convert back to Image a with ext
-- mconcatOver ext inj bms = fmap getAll (mconcat (map (fmap All) bms))
mconcatOver ext inj = fmap ext . mconcat . map (fmap inj)

mappendOver :: (Functor f, Semigroup (f a1)) => (a1 -> b) -> (a2 -> a1) -> f a2 -> f a2 -> f b
-- mappendOver :: Monoid b => (b -> a) -> (a -> b) -> Image a -> Image a -> Image a
mappendOver ext inj bm1 bm2 = fmap ext (fmap inj bm1 <> fmap inj bm2)

-- TODO: these would be useful for color images too if colors were monoids... check on this..
-- if so, we can make sum and products of images easily with monoid.

-- mulBitmaps returns a bitmap that is True everywhere all input bitmaps are True.
mulBitmaps :: [Bitmap] -> Bitmap
mulBitmaps = mconcatOver getAll All

mulBitmap :: Bitmap -> Bitmap -> Bitmap
-- mulBitmap bm1 bm2 = mappendOver getAll All bm1 bm2
-- or more simply:
mulBitmap = liftA2 (&&)

sumBitmaps :: [Bitmap] -> Bitmap
sumBitmaps = mconcatOver getAny Any

sumBitmap :: Bitmap -> Bitmap -> Bitmap
-- sumBitmap = mappendOver getAny Any
sumBitmap = liftA2 (||)

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage = Image . const

-- transforms an image's coordinates by f.
-- note: the image will appear transformed by the inverse of f.
-- but for simplicity I'm not going to try to define inverse functions.
transformImage :: (Coord -> Coord) -> Image a -> Image a
transformImage f (Image imgf) = Image (imgf . f)

-- translate an image by (dx, dy).
translateImage :: Coord -> Image a -> Image a
translateImage (dx, dy) = transformImage $ translate (-dx, -dy)

-- scale an image's coordinates by s.
-- Note this is different than `scale` on images which scales image values.
scaleImage :: Float -> Image a -> Image a
scaleImage s = transformImage $ scaleF (1/s)

-- transform coordinates to display image at the origin from first quadrant coordinates.
originImage :: Image a -> Image a
originImage = transformImage unitToOrigin

-- transform coordinates to display image in first quadrant from centered coordinates.
unoriginImage :: Image a -> Image a
unoriginImage = transformImage originToUnit

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

-- constAnim is an animation of a constant image.
constAnim :: Image a -> Anim a
constAnim img = Anim (\_ts -> img)

-- mapAnim runs f over each animated image.
-- fmap on an Anim will run f on each value from each image in Anim.
instance Functor Anim where
  -- at each ts we get the image and map f over it.
  -- Anim (\ts -> fmap f (an ts))
  fmap f (Anim an) = Anim (fmap f . an)

instance Applicative Anim where
  pure v = mkAnim (\_ts _coord -> v)
  (Anim anf1) <*> (Anim anf2) = Anim (\ts -> anf1 ts <*> anf2 ts)

instance Semigroup a => Semigroup (Anim a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Anim a) where
  mempty = constAnim mempty

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
