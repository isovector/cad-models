module ClockEnclosure where

import Graphics.Implicit
import Lib

------------------------------------------------------------------------------
-- |  ASSUME THE CAMERA IS FROM THE FRONT


------------------------------------------------------------------------------
-- | z is UP


main :: IO ()
main = do
  writeSTL 1 "/tmp/res.stl" $ do
  -- writePNG3 1 "/tmp/res.png" $ do
    top

base :: SymbolicObj3
base =
  union
    [ plate
    , difference [walls, viewhole, cordhole]
    , wedge depth width vh_offset
    ]

top :: SymbolicObj3
top =
  union
    [ plate
    , nubs
    ]


width :: R
width = 103.58 + tolerance

height :: R
height = 32.70 + height_padding + tolerance + height_padding / 2

depth :: R
depth = 48.31

thickness :: R
thickness = 2.5


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

nub_size :: R
nub_size = 7

nub_height :: R
nub_height = 10

nubs :: SymbolicObj3
nubs = union
  [ rect3R 0
      (mk3 0 0 0)
      (mk3 nub_size        nub_size            nub_height)
  , rect3R 0
      (mk3 0 (depth - nub_size) 0)
      (mk3 nub_size (depth)        nub_height)
  , rect3R 0
      (mk3 (width - nub_size) 0 0)
      (mk3 width nub_size nub_height)
  , rect3R 0
      (mk3 (width - nub_size) (depth - nub_size) 0)
      (mk3 (width)        (depth) nub_height)
  ]

tolerance :: R
tolerance = 5

vh_offset :: R
vh_offset = 1.9 + height_padding

vh_height :: R
vh_height = 30.36 + tolerance

vh_inlay :: R
vh_inlay = 5.4

height_padding :: R
height_padding = 14.45


viewhole :: SymbolicObj3
viewhole =
  rect3R 0
    (mk3 vh_inlay 0 vh_offset)
    (mk3 (width - vh_inlay) (depth + thickness) (vh_offset + vh_height))


cordhole :: SymbolicObj3
cordhole = translate (mk3 cord_inlay thickness cord_height) $ rotate3 (degX 90) $ cylinder cord_rad depth

cord_rad :: R
cord_rad = 6

cord_height :: R
cord_height = 23.67

cord_inlay :: R
cord_inlay = 7.6


