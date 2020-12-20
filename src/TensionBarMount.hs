module TensionBarMount where

import Graphics.Implicit
import Lib


main :: IO ()
main = writeSTL 0.25 "/tmp/mount.stl" $
  let base = center3 $ union
        [ slamBack $ slamTop $ centeredBox 40 40 6
        , slamBack $ slamBottom $ centeredBox 9 3.7 6
        ]
   in difference
        ( slamFront $ difference
          ( slamBottom $ union
            [ mirror (V3 1 1 0) base
            , mirror (V3 (-1) 1 0) base
            , base
            ])
          [ translate (mk3 0 0 (-6))
              $ slamBottom
              $ cylinder (33 / 2) 10
          , translate (mk3 0 (-3) (-6))
              $ slamBottom
              $ cylinder (33 / 2) 10
          ])
        [ slamFront $ slamBottom $ centeredBox 40 14.5 6
        ]


