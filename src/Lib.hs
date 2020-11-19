{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Lib
  ( module Lib
  , module Graphics.Implicit
  , L.V2 (..)
  , L.V3 (..)
  , L.M22
  , L.M33
  , L.M44
  , (L.!*)
  , (L.*!)
  , (L.^*)
  , (L.*^)
  , (L.!*!)
  , (L.!+!)
  , (L.!-!)
  , L._x
  , L._y
  , L._z
  , module Control.Lens
  ) where

import           Control.Lens hiding (plate)
import           Graphics.Implicit
import           Graphics.Implicit.Primitives (Object(getBox))
import qualified Linear as L
import           Linear.Matrix
import           Linear.V2 hiding (R2)


type R = Double
type R2 = (R, R)
type R3 = (R, R, R)


instance Semigroup Double where
  (<>) = (+)

instance Semigroup SymbolicObj3 where
  a <> b = union [a,  b]

instance Semigroup SymbolicObj2 where
  a <> b = union [a,  b]

instance Monoid SymbolicObj2 where
  mempty = rectR 0 zero zero

instance Monoid SymbolicObj3 where
  mempty = rect3R 0 zero zero

extrude :: R -> SymbolicObj2 -> SymbolicObj3
extrude = flip $ extrudeR 0

wedge
    :: R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
wedge w d h
  = rotate3 (degZ 90)
  $ rotate3 (degX 90)
  $ extrude d
  $ polygonR 0 [mk2 0 0, mk2 w 0, mk2 w h]

mk2 :: R -> R -> R2
mk2 = (,)

mk3 :: R -> R -> R -> R3
mk3 = (,,)

box
    :: R  -- ^ width
    -> R  -- ^ height
    -> R  -- ^ depth
    -> SymbolicObj3
box w h d = rect3R 0 (0, 0, 0) (w, h, d)

centeredBox
    :: R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
centeredBox = centeredBoxR 0

centeredBoxR
    :: R
    -> R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
centeredBoxR r w d h =
  rect3R r
    (-half_w, -half_d, -half_h)
    (half_w, half_d, half_h)
  where
    half_w = w / 2
    half_d = d / 2
    half_h = h / 2

deg :: R -> R
deg degs = degs * pi / 180

degX :: R -> R3
degX d = mk3 (deg d) 0 0

degY :: R -> R3
degY d = mk3 0 (deg d) 0

degZ :: R -> R3
degZ d = mk3 0 0 (deg d)


extrudedSlot
    :: R  -- ^ slot thickness
    -> R  -- ^ slot height
    -> SymbolicObj3
    -> SymbolicObj3
extrudedSlot th h obj =
  let obj' = shell th obj
      ((x, y, z), (x', y', _)) = getBox obj'
      obj'' = translate (0, 0, negate z - th - 1) obj'
   in intersect
        [ obj''
        , rect3R 0 (x, y, 0) (x', y', h)
        ]


unpackV2 :: L.V2 a -> (a, a)
unpackV2 (L.V2 x y) = (x, y)

unpackV3 :: L.V3 a -> (a, a, a)
unpackV3 (L.V3 x y z) = (x, y, z)

packV2 :: (a, a) -> L.V2 a
packV2 (x, y) = (L.V2 x y)

packV3 :: (a, a, a) -> L.V3 a
packV3 (x, y, z) = (L.V3 x y z)


expandR2 :: R -> R2 -> R3
expandR2 z (x, y) = (x, y, z)


withPolarPos
    :: R             -- ^ r
    -> R             -- ^ theta in degrees
    -> SymbolicObj3  -- ^ obj to position
    -> SymbolicObj3
withPolarPos r theta = translate (polarPos r $ deg theta)


polarSymmetrically :: R -> R -> SymbolicObj3 -> SymbolicObj3
polarSymmetrically r theta obj = union
  [ withPolarPos r theta obj
  , withPolarPos r (-theta) obj
  ]



polarPos
    :: R  -- ^ r
    -> R  -- ^ theta
    -> R3
polarPos r theta =
  expandR2 0 $ unpackV2 $ rotMat theta !* V2 0 r


rotMat :: R -> L.M22 Double
rotMat theta =
  V2 (V2 ct (-st))
     (V2 st ct)
  where
    ct = cos theta
    st = sin theta


getOrigin :: Object obj vec => obj -> vec
getOrigin = fst . getBox

getExtent :: Object obj vec => obj -> vec
getExtent = snd . getBox

getSize :: SymbolicObj3 -> R3
getSize obj = getExtent obj - getOrigin obj


expand :: R3 -> SymbolicObj3 -> SymbolicObj3
expand (dx, dy, dz) obj =
  let (w, d, h) = getSize obj
      wf = (w + dx) / w
      df = (d + dy) / d
      hf = (h + dz) / h
   in scale (wf,df, hf) obj



class StupidImplicitVector a where
  zero :: a

instance StupidImplicitVector R2 where
  zero = mk2 0 0

instance StupidImplicitVector R3 where
  zero = mk3 0 0 0



slamming
    :: (Num a, Object obj vec, StupidImplicitVector vec)
    => Lens' vec a
    -> (obj -> vec)
    -> obj
    -> obj
slamming l boxsel obj =
  let dist = negate $ boxsel obj ^. l
      dpos = zero & l .~ dist
   in translate dpos obj


------------------------------------------------------------------------------
-- | Put it at z=0
slamBottom :: SymbolicObj3 -> SymbolicObj3
slamBottom = slamming _3 getOrigin

slamTop :: SymbolicObj3 -> SymbolicObj3
slamTop = slamming _3 getExtent

slamLeft :: (Object obj vec, StupidImplicitVector vec, Field1 vec vec Double Double) => obj -> obj
slamLeft = slamming _1 getOrigin

slamRight :: (Object obj vec, StupidImplicitVector vec, Field1 vec vec Double Double) => obj -> obj
slamRight = slamming _1 getExtent

slamFront :: (Object obj vec, StupidImplicitVector vec, Field2 vec vec Double Double) => obj -> obj
slamFront = slamming _2 getOrigin

slamBack :: (Object obj vec, StupidImplicitVector vec, Field2 vec vec Double Double) => obj -> obj
slamBack = slamming _2 getExtent

center3 :: SymbolicObj3 -> SymbolicObj3
center3 obj =
  let (packV3 -> orig, packV3 -> ext) = getBox obj
      dpos = negate $ orig + (ext - orig)  L.^* 0.5
   in translate (unpackV3 dpos) obj


split :: SymbolicObj3 -> [SymbolicObj3]
split obj =
  let ((x1, y1, z1), (x2, y2, z2)) = getBox obj
      w = x2 - x1
      d = y2 - y1
      h = z2 - z1
      b = centeredBox w d h
   in [ intersect [ obj, translate (mk3 x1 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y2 z1) b ]
      , intersect [ obj, translate (mk3 x1 y2 z1) b ]
      ]


