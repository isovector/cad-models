module Types
  ( module Graphics.Implicit
  , module Types
  ) where

import Graphics.Implicit
import qualified Linear as L

type R = Double
type R2 = (R, R)
type R3 = (R, R, R)


mk2 :: R -> R -> R2
mk2 = (,)

mk3 :: R -> R -> R -> R3
mk3 = (,,)


unpackV2 :: L.V2 a -> (a, a)
unpackV2 (L.V2 x y) = (x, y)

unpackV3 :: L.V3 a -> (a, a, a)
unpackV3 (L.V3 x y z) = (x, y, z)

packV2 :: (a, a) -> L.V2 a
packV2 (x, y) = (L.V2 x y)

packV3 :: (a, a, a) -> L.V3 a
packV3 (x, y, z) = (L.V3 x y z)
