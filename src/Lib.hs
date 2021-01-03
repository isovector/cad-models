{-# LANGUAGE LambdaCase #-}
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
  , module Types
  , module Alignment
  , carve
  , inverse
  ) where

import           Alignment
import           Control.Lens hiding (plate, both)
import           Graphics.Implicit
import           Graphics.Implicit.Primitives (Object(getBox))
import qualified Linear as L
import           Linear.Matrix
import           Merge (carve, inverse)
import           Types


instance Semigroup Double where
  (<>) = (+)

wedge
    :: R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
wedge w d h
  = rotate3 (degZ 90)
  $ rotate3 (degX 90)
  $ flip extrude d
  $ polygon [mk2 0 0, mk2 w 0, mk2 w h]

box
    :: R  -- ^ width
    -> R  -- ^ height
    -> R  -- ^ depth
    -> SymbolicObj3
box w h d = rect3 (pure 0) (mk3 w h d)

centeredBox
    :: R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
centeredBox w d h =
  rect3
    (mk3 (-half_w) (-half_d) (-half_h))
    (mk3 half_w half_d half_h)
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
  let spacing = th + 1
      obj' = shell th $ expand (mk3 spacing spacing h) obj
      ((V3 x y z), (V3 x' y' _)) = getBox obj'
      obj'' = translate (mk3 0 0 $ negate z - th - 1) obj'
   in intersect
        [ obj''
        , rect3 (mk3 x y 0) (mk3 x' y' h)
        ]

expandR2 :: R -> R2 -> R3
expandR2 z (V2 x y) = mk3 x y z


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
  expandR2 0 $ rotMat theta !* V2 0 r


rotMat :: R -> L.M22 Double
rotMat theta =
  V2 (V2 ct (-st))
     (V2 st ct)
  where
    ct = cos theta
    st = sin theta


getSize :: SymbolicObj3 -> R3
getSize obj = getExtent obj - getOrigin obj


expand :: R3 -> SymbolicObj3 -> SymbolicObj3
expand (V3 dx dy dz) obj =
  let (V3 w d h) = getSize obj
      wf = (w + dx) / w
      df = (d + dy) / d
      hf = (h + dz) / h
   in scale (mk3 wf df hf) obj


pyramid :: R -> R -> R -> SymbolicObj3
pyramid w d h =
  extrudeM
    (Left 0)
    (Fn $ \x -> Left $ max 0.01 $ (h - x) / h)
    (Left 0)
    (rect (mk2 (-half w) (-half d)) (mk2 (half w) (half d)))
    (Left h)
  where
    half x = x / 2

translateXY :: R -> R -> SymbolicObj3 -> SymbolicObj3
translateXY x y = translate (mk3 x y 0)


holderR
  :: R  -- ^ thickness
  -> R
  -> R
  -> R
  -> SymbolicObj3
holderR th x y z =
  difference (slamTop $ centeredBox (x + th * 2) (y + th * 2) z)
    [ slamTop $ centeredBox x y z
    ]

testSlice :: Alignment -> R -> SymbolicObj3 -> SymbolicObj3
testSlice a th s =
  intersect
    [ slam a $ cube True $ mk3 500 500 th
    , slam a s
    ]


roundedPlate
    :: R  -- ^ rounding on "flat" side
    -> R  -- ^ rounding on "rounded" side
    -> R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ thickness
    -> SymbolicObj3
roundedPlate r1 r2 x y z = slamTop $
  center3
    $ flip extrude z
    $ union
    $ let half = y / 2
       in [ slamBack $ withRounding r2 $ rect 0 $ mk2 x y
          , slamBack $ withRounding r1 $ rect 0 $ mk2 x half
          ]

mirrored :: R3 -> SymbolicObj3 -> SymbolicObj3
mirrored v obj = obj <> mirror v obj

reflected :: SymbolicObj3 -> SymbolicObj3
reflected = mirrored $ mk3 1 0 0

