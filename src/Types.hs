module Types
  ( module Graphics.Implicit
  , module Types
  ) where

import Graphics.Implicit
import qualified Linear as L

type R = Double
type R2 = L.V2 R
type R3 = L.V3 R


mk2 :: R -> R -> R2
mk2 = L.V2

mk3 :: R -> R -> R -> R3
mk3 = L.V3

