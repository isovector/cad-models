module StdParts where

import Lib
import Graphics.Implicit.Primitives


yellowWheelBB :: SymbolicObj3
yellowWheelBB =
  centeredBox
    wheel_width
    wheel_diameter
    wheel_diameter
  where
    wheel_width = 28
    wheel_diameter = yellowWheelRadius * 2


yellowWheelRadius :: R
yellowWheelRadius = 70 / 2


yellowWheelMotorWidth :: R
yellowWheelMotorWidth = 19

yellowWheelMotorAxelOffset :: R
yellowWheelMotorAxelOffset = 11.3

yellowWheelMotorSlot :: SymbolicObj3
yellowWheelMotorSlot = extrudedSlot 2 2 yellowWheelMotorBB

yellowWheelMotorBB :: SymbolicObj3
yellowWheelMotorBB = centeredBox 19 48.3 22.5


tabXPosFacing :: SymbolicObj3
tabXPosFacing = box 2 5 7


tabXNegFacing :: SymbolicObj3
tabXNegFacing = rotate3 (degZ pi) $ box 2 5 7


arduinoMiniSlot :: SymbolicObj3
arduinoMiniSlot =
  slamBottom $ center3 $
    mconcat
      [ slamLeft $ extrudedSlot 2 2 $ centeredBox 45 18.5 10
        -- usb port
      , inverse $ translate (mk3 (-5) 0 0) $ slamBottom $ slamLeft $ centeredBox 10 8 4.4
      ]


l298nSlot :: SymbolicObj3
l298nSlot = mconcat
  [ extrudedSlot 2 2 $ centeredBox 43.5 43.5 10
  , corners (43.5 / 2 - 3) (43.5 / 2 - 3) m3Hole
  ]


miniBreadboardSlot :: SymbolicObj3
miniBreadboardSlot = extrudedSlot 2 2 $ centeredBox 35 47 10


doubleAAHolderSlot :: SymbolicObj3
doubleAAHolderSlot = extrudedSlot 2 2 $ centeredBox 32.4 85.5 10


ovonicLipoBatterySlot :: SymbolicObj3
ovonicLipoBatterySlot = slamBottom $ center3 $ mconcat
  [ slamFront $ slamRight $ extrudedSlot 2 2 $
      centeredBox 35 68 29
    -- charging port
  , inverse $ translate (mk3 5 10 $ 29+2) $ slamBottom $ slamFront $ slamRight $ centeredBox 10 13.5 4
  ]


quarter :: SymbolicObj3 -> [SymbolicObj3]
quarter obj =
  let ((V3 x1 y1 z1), (V3 x2 y2 z2)) = getBox obj
      w = x2 - x1
      d = y2 - y1
      h = z2 - z1
      b = centeredBox w d h
   in [ intersect [ obj, translate (mk3 x1 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y2 z1) b ]
      , intersect [ obj, translate (mk3 x1 y2 z1) b ]
      ]

corners :: R -> R -> SymbolicObj3 -> SymbolicObj3
corners x y obj = union
  [ translateXY   x    y  obj
  , translateXY (-x)   y  obj
  , translateXY (-x) (-y) obj
  , translateXY   x  (-y) obj
  ]

m3MountingPlate :: SymbolicObj3
m3MountingPlate = slamBottom $ center3 $ carve $
  mconcat
    [ slamLeft $
        mconcat
          [ slamBottom $ centeredBox 8 8 2
          , translate (mk3 0 0 $ -2) $ slamBottom $ inverse $ center3 $ cylinder (3.1/2) 5
          ]
    , slamRight $ slamBottom $ centeredBox 2 8 8
    ]

m3Hole :: SymbolicObj3
m3Hole = inverse $ center3 $ cylinder (3.2/2) 5

