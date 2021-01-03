module Gear where

import Lib
import StdParts
import Merge (merging)


main :: IO ()
main = do
  writeSTL 0.1 "/tmp/roomba.stl" $
    difference
      (slamBottom $ mconcat
        [ slamTop $ gear' 28 (14.48 / 2) 1.75
        , slamBottom $ gear' 13 (8.08 / 2) 3.5
        ])
      [ cylinder (2.3 / 2) 6
      ]


gear' :: Int -> R -> R -> SymbolicObj3
gear' n rad th =
  let r = rad + 0.4
   in flip extrude th $ gear n 1 (r - 1.90) (r + 0.7)


gear
  :: Int  -- ^ num teeth
  -> R    -- ^ tooth thickness
  -> R    -- ^ inner radius
  -> R    -- ^ outer radius
  -> SymbolicObj2
gear n th r0 r = polygon $ do
  let dtheta = 2 * pi / fromIntegral n
      half_th = th / 2
      tooth_arc_len = half_th / r0
  i <- fromIntegral <$> [0 .. n - 1]
  let theta = i * dtheta
  [   V2 (cos (theta - tooth_arc_len) * r0) (sin (theta - tooth_arc_len) * r0)
    , V2 (cos theta * r) (sin theta * r)
    , V2 (cos (theta + tooth_arc_len) * r0) (sin (theta + tooth_arc_len) * r0)
    ]



sliver :: R -> R -> R
sliver r smaller = smaller / (2 * pi * r)

