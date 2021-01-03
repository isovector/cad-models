module Boxes where

import Lib
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions


-- main :: IO ()
-- main = writeSTL 1 "/tmp/roomba2.stl" $
--   center3 $ withoutCubeFaces [OnTop] 120 120 20 2

cubeFaces
    :: [Alignment]
    -> R  -- ^ w
    -> R  -- ^ d
    -> R  -- ^ h
    -> R  -- ^ th
    -> SymbolicObj3
cubeFaces fs x y z th = carve $ mconcat $
  [ inverse $ centeredBox x y z                   ] <>
  [ left_plane                     | has OnLeft   ] <>
  [ mirror (V3 1 0 0) left_plane   | has OnRight  ] <>
  [ front_plane                    | has OnFront  ] <>
  [ mirror (V3 0 1 0) front_plane  | has OnBack   ] <>
  [ bottom_plane                   | has OnBottom ] <>
  [ mirror (V3 0 0 1) bottom_plane | has OnTop    ]
  where
    has = flip elem fs
    tlfv = V3 (-halfx) (-halfy) (-halfz)

    left_plane   = rect3 (tlfv - V3 th th th) (V3 (-halfx) halfy halfz + V3 0 th th)
    front_plane  = rect3 (tlfv - V3 th th th) (V3 halfx (-halfy) halfz + V3 th 0 th)
    bottom_plane = rect3 (tlfv - V3 th th th) (V3 halfx halfy (-halfz) + V3 th th 0)

    halfx = x / 2
    halfy = y / 2
    halfz = z / 2

withoutCubeFaces
    :: [Alignment]
    -> R  -- ^ w
    -> R  -- ^ d
    -> R  -- ^ h
    -> R  -- ^ th
    -> SymbolicObj3
withoutCubeFaces fs = cubeFaces (filter (not . flip elem fs) [minBound .. maxBound])


atSamePlace :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
atSamePlace (Shared (Translate v _)) obj = translate v obj
atSamePlace _ obj = obj


container
    :: R -- ^ w
    -> R -- ^ d
    -> R -- ^ h
    -> R -- ^ th
    -> SymbolicObj3
container = withoutCubeFaces [OnTop]

insideContainer
    :: R -- ^ w
    -> R -- ^ d
    -> R -- ^ h
    -> R -- ^ th
    -> SymbolicObj3
insideContainer x y z th = container (x - 2 * th) (y - 2 * th) (z - th) th


barrel
    :: R  -- ^ radius
    -> R  -- ^ height
    -> R  -- ^ thickness
    -> SymbolicObj3
barrel r h th = carve $
  allFlush OnTop
    [ cylinder r h
    , inverse $ cylinder (r - th) (h - th)
    ]

tube
    :: R  -- ^ radius
    -> R  -- ^ height
    -> R  -- ^ thickness
    -> SymbolicObj3
tube r h th =
  difference
      (cylinder r h)
      [ cylinder (r - th) h ]

