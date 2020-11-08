module Main where

import Graphics.Implicit
import Lib

------------------------------------------------------------------------------
-- |  ASSUME THE CAMERA IS FROM THE FRONT


------------------------------------------------------------------------------
-- | z is UP


main :: IO ()
main = do
  writeSTL 0.1 "/tmp/res.stl" $ do
  -- writePNG3 1 "/tmp/res.png" $ do
    union
      [ plate
      , difference [walls, viewhole, cordhole]
      , wedge width depth vh_offset
      ]
    -- rotate3 (degX 45 <> degY 45) $ intersect
    --   [ rotate3 (degX 30) b
    --   , shell 1 b
    --   ]


width :: R
width = 10

height :: R
height = 10

depth :: R
depth = 10

thickness :: R
thickness = 1


plate :: SymbolicObj3
plate =
  rect3R 0
    (mk3 (-thickness) (-thickness) (-thickness))
    (mk3 (width + thickness) (depth + thickness) 0)

walls :: SymbolicObj3
walls = union
  [ rect3R 0
      (mk3 (-thickness) (-thickness) 0)
      (mk3 (width + thickness)        0            height)
  , rect3R 0
      (mk3 (-thickness) (-thickness) 0)
      (mk3 0            (depth + thickness)        height)
  , rect3R 0
      (mk3 width        (-thickness) 0)
      (mk3 (width + thickness) (depth + thickness) height)
  , rect3R 0
      (mk3 (-thickness) depth 0)
      (mk3 (width + thickness)        (depth + thickness) height)
  ]

vh_offset :: R
vh_offset = 2

vh_height :: R
vh_height = 2

vh_inlay :: R
vh_inlay = 1


viewhole :: SymbolicObj3
viewhole =
  rect3R 0
    (mk3 vh_inlay 0 vh_offset)
    (mk3 (width - vh_inlay) (depth + thickness) (vh_offset + vh_height))


cordhole :: SymbolicObj3
cordhole = translate (mk3 cord_inlay thickness cord_height) $ rotate3 (degX 90) $ cylinder cord_rad depth

cord_rad :: R
cord_rad = 1

cord_height :: R
cord_height = 4

cord_inlay :: R
cord_inlay = 2


