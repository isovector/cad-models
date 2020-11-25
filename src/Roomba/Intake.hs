module Roomba.Intake where

import Lib
import StdParts
import Merge (merging)



main :: IO ()
main = do
  writeSTL 1 "/tmp/roomba.stl" $ do
    intakeStock

intakeStock :: SymbolicObj3
intakeStock =
  merging
    ( slamBack $ slamBottom $ differenceR 2
      [ union
          [ slamTop $ centeredBox (120 + 6) (120 + 6) (25 + 3)
          , slamTop $ cylinder ((115 + 6) / 2) 53
          ]
      , slamTop $ centeredBox 120 120 25
      , slamTop $ cylinder (115 / 2) 50
      , corners (120 / 2 - 7) (120 / 2 - 7) $ slamTop $ cylinder (4.5 / 2) 50
      ]
    )
    [ slamBack $ difference
      [ slamBottom $ centeredBox (30 + 6) 15 (10 + 6)
      , translate (mk3 0 0 3) $ slamBottom $ centeredBox (30) 15 (10)
      ]
    ]




corners :: R -> R -> SymbolicObj3 -> SymbolicObj3
corners x y obj = union
  [ translateXY   x    y  obj
  , translateXY (-x)   y  obj
  , translateXY (-x) (-y) obj
  , translateXY   x  (-y) obj
  ]


