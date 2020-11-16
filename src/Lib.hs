module Lib where

import Graphics.Implicit
import Graphics.Implicit.Primitives (Object(getBox))


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
      obj'' = translate (0, 0, negate $ z + th) obj'
   in intersect
        [ obj''
        , rect3R 0 (x, y, 0) (x', y', h)
        ]

