module Roomba.Intake where

import Lib
import Merge (merging, carve)



main :: IO ()
main = do
  writeSTL 2 "/tmp/roomba.stl" $ do
    suckHole

intakeStock :: SymbolicObj3
intakeStock =
  merging
    ( slamBack $ slamBottom $ differenceR 2
      ( union
          [ slamTop $ centeredBox (120 + 6) (120 + 6) (25 + 3)
          , slamTop $ cylinder ((115 + 6) / 2) 53
          ]
      )
      [ slamTop $ centeredBox 120 120 25
      , slamTop $ cylinder (115 / 2) 50
      , corners (120 / 2 - 7) (120 / 2 - 7) $ slamTop $ cylinder (4.5 / 2) 50
      ]
    )
    [ slamBack $ difference
      (slamBottom $ centeredBox (30 + 6) 15 (10 + 6))
      [ translate (mk3 0 0 3) $ slamBottom $ centeredBox (30) 15 (10)
      ]
    ]


corners :: R -> R -> SymbolicObj3 -> SymbolicObj3
corners x y obj = union
  [ translateXY   x    y  obj
  , translateXY (-x)   y  obj
  , translateXY (-x) (-y) obj
  , translateXY   x  (-y) obj
  ]


suckHole :: SymbolicObj3
suckHole = carve $
  merging
    ( slamBottom $ merging
        ( slamTop
            $ translateXY 0 (-60)
            $ rotate3 (degZ $ 45)
            $ rotate3 (degY $ -90)
            $ wedgeHolder 2 80 18 80
        )
        [ fan
        ])
    [ translateXY 0 (-50) $ slamBottom $ slamBack $ containerBB
    ]

containerBB :: SymbolicObj3
containerBB = difference
  ( union
      [ translateXY 0 (-2) $ slamBottom $ slamFront $
          union
            [ slamTop $ holderR 0 2 118 50 20
            -- -- TODO(sandy): remove this to make a container hole
            -- , slamBottom $ centeredBox 122 54 2
            ]
      , slamBottom $ slamBack  $
          difference
            ( union
              [ slamTop $ holderR 0 2 158 15 18
              , slamBottom $ centeredBox 162 19 2
              ])
            [ slamTop $ centeredBox 158 15 40
            ]
      ]
  )
  [ slamBottom $ slamBack $ centeredBox 40 15 18
  ]

wedgeHolder :: R -> R -> R -> R -> SymbolicObj3
wedgeHolder th w d h =
  difference (slamLeft $ center3 $ wedge (w + 2 * th)  (d + th) (h + 2 * th))
    [ slamLeft $ center3 $ wedge w  d h
    ]

tube :: R -> R -> R -> SymbolicObj3
tube th r h =
  difference (cylinder (r + th) h) [ cylinder r h ]

exhaust :: SymbolicObj3
exhaust = translateXY (-64) 60 $ slamLeft $ slamBottom $ box 62 200 32.25


-- | Contains an invisible exhaust hole
fan :: SymbolicObj3
fan = difference (slamTop $ tube 2 (77 / 2) 20) [exhaust]

-- 120x120x32.25

