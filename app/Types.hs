-- Types for the images, bitmaps, and animations.
module Types (
  Time

  , Coord
  , mag
  , ang
  , translate
  , scale
  , unitToOrigin
  , originToUnit
  , rotate

  , Image
  , constImage
  , transformImage
  , translateImage
  , scaleImage
  , originImage
  , unoriginImage
  , rotateImage

  , Bitmap
  , circle
  , square
  , checkerboard
  , andBitmap
  , andBitmaps
  , orBitmap
  , orBitmaps
  , mixImage
  , maskImage

  , ColorImage
  , colorBitmap
  , bwBitmap

  , Anim
  , constAnim
  , mapImageValues
  , warpTime
  , speedUp
  , fastForward

  , ColorAnim

  -- re-exports from Gloss.
  , FromFloat
  , fromFloat
  , Color
  , black
  , white
  , getRGBA
  , rgba
  , rgb
  , anim
) where

import Control.Applicative

import Gloss

-- Time is a real.
type Time = Float

-- Coordinates are pairs of reals.
type Coord = (Float, Float)

mag :: Coord -> Float
mag (x, y) = sqrt (x*x + y*y)

ang :: Coord -> Float
ang (x, y) = atan2 y x

translate :: Coord -> Coord -> Coord
translate (dx, dy) (x,y) = (x + dx, y + dy)

scale :: Float -> Coord -> Coord
scale s (x, y) = (x*s, y*s)

unitToOrigin :: Coord -> Coord
unitToOrigin = translate (-1, -1) . scale 2

originToUnit :: Coord -> Coord
originToUnit = scale 0.5 . translate (1, 1)

rotate :: Float -> Coord -> Coord
rotate theta (x, y) = (x * costheta - y * sintheta, x * sintheta + y * costheta)
  where
    costheta = cos theta
    sintheta = sin theta

-- An Image maps coordinates to values.
-- It is a functor, and an applicative functor (because (->) are).
-- `fmap f image` applies f to each value in image.
-- `(+) <$> image1 <*> image2` does a pair-wise addition of each value in image1 and image2.
type Image a = Coord -> a

-- constImage returns an image where all (x,y) values are v.
constImage :: a -> Image a
constImage a _pos = a

-- transforms an image's coordinates by f.
-- note: the image will appear transformed by the inverse of f.
-- but for simplicity I'm not going to try to define inverse functions.
transformImage :: (Coord -> Coord) -> Image a -> Image a
transformImage f imgf = imgf . f

-- translate an image by (dx, dy).
translateImage :: Coord -> Image a -> Image a
translateImage (dx, dy) = transformImage $ translate (-dx, -dy)

-- scale an image's coordinates by s.
scaleImage :: Float -> Image a -> Image a
scaleImage s = transformImage $ scale (1/s)

-- transform coordinates to display image at the origin from first quadrant coordinates.
originImage :: Image a -> Image a
originImage = transformImage unitToOrigin

-- transform coordinates to display image in first quadrant from centered coordinates.
unoriginImage :: Image a -> Image a
unoriginImage = transformImage originToUnit

rotateImage :: Float -> Image a -> Image a
rotateImage theta = transformImage $ rotate (0-theta)

-- A bitmap is an image of Bools, mapping each coordinate to a True or False value.
type Bitmap = Image Bool

-- the unit circle
circle :: Bitmap
circle pos = mag pos < 1

-- the first quadrant square
square :: Bitmap
square (x, y) = (0 <= x && x < 1) && (0 <= y && y < 1)

-- an infinite checkerboard with n x n squares in the first quadrant.
checkerboard :: Int -> Bitmap
checkerboard n (x, y) = even (tileNum x + tileNum y)
  where
    -- tileNum returns the tile number for v where there are n tiles from [0..1].
    tileNum :: Float -> Int
    tileNum v = floor (v * fromIntegral n)

-- andBitmaps combines two bitmaps resulting in values that are True where both bitmaps are True.
andBitmap :: Bitmap -> Bitmap -> Bitmap
andBitmap = liftA2 (&&)

-- andBitmaps returns a bitmap that is True everywhere all input bitmaps are True.
andBitmaps :: [Bitmap] -> Bitmap
andBitmaps bms pos = and [bm pos | bm <- bms]

orBitmap :: Bitmap -> Bitmap -> Bitmap
orBitmap = liftA2 (||)

orBitmaps :: [Bitmap] -> Bitmap
orBitmaps bms pos = or [bm pos | bm <- bms]

-- A ColorImage is an image of Colors, mapping each coordinate to a Color.
type ColorImage = Image Color

-- colorBitmap returns a color image with bg color when bm is false and fg color when bm is true.
colorBitmap :: Color -> Color -> Bitmap -> ColorImage
colorBitmap bg fg bm pos = if bm pos then fg else bg

-- bwBitmap converts a bitmap to a black and white color image.
bwBitmap :: Bitmap -> ColorImage
bwBitmap = colorBitmap black white

-- mixImage shows background image bg where bm is false and foreground image fg where bm is true.
mixImage :: Bitmap -> Image a -> Image a -> Image a
mixImage = liftA3 (\bmv bgv fgv -> if bmv then fgv else bgv)

-- maskImage shows image fg where bm is true, and zero when bm is false.
maskImage :: Num a => Bitmap -> Image a -> Image a
maskImage = liftA2 (\bmv fgv -> if bmv then fgv else 0)


-- An Anim maps time to images.
-- It is a functor and an applicative functor (because of (-> a)).
-- `fmap f an` applies f to each image in the animation.
-- `binop <$> an1 <*> an2` mixes an1 with an2 at each time using binop.
type Anim a = Time -> Image a

-- constAnim is an animation of a constant image.
constAnim :: Image a -> Anim a
constAnim img _ts = img

-- mapImageValues maps f over each value in each image.
mapImageValues :: (a -> b) -> Anim a -> Anim b
mapImageValues = fmap . fmap

-- warpTime uses (f time) as the time in the animation.
warpTime :: (Time -> Time) -> Anim a -> Anim a
warpTime f anf = anf . f

-- speedUp speeds up the animation by s.
speedUp :: Float -> Anim a -> Anim a
speedUp s = warpTime (*s)

-- fastForward skips the animation forward by dt.
fastForward :: Time -> Anim a -> Anim a
fastForward dt = warpTime (+dt)

-- A ColorAnim is animation of Color images.
-- It maps timestamps in seconds and (x,y) to Colors.
type ColorAnim = Anim Color
