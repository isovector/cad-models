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


type R = Double
type R2 = (R, R)
type R3 = (R, R, R)


instance Semigroup Double where
  (<>) = (+)

extrude :: R -> SymbolicObj2 -> SymbolicObj3
extrude = flip $ extrudeR 0

wedge
    :: R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ height
    -> SymbolicObj3
wedge w d h = rotate3 (degZ 90) $ rotate3 (degX 90) $ extrude d $ polygonR 0 [mk2 0 0, mk2 w 0, mk2 w h]

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

expandR2 :: R -> R2 -> R3
expandR2 z (x, y) = (x, y, z)


