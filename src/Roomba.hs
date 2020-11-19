module Roomba where

import Lib
import StdParts


main :: IO ()
main = do
  writeSTL 0.5 "/tmp/roomba.stl" $ do
    basePlate
    -- union
    --   [ head $ split basePlate
    --   ]

baseThickness :: R
baseThickness = 2.5

thickness :: R
thickness = 1.5


plateSize :: R
plateSize = 180


stockPlate :: SymbolicObj3
stockPlate
  = slamTop
  $ center3
  $ extrude baseThickness
  $ union
  $ let half = plateSize / 2
     in [ slamBack $ rectR half zero $ mk2 plateSize plateSize
        , slamBack $ rectR 0 zero $ mk2 plateSize half
        ]

basePlate :: SymbolicObj3
basePlate = slamBottom $
  difference
    [ union
        [ stockPlate
        , translate (mk3 (-wheelXOffset) (wheelYOffset - yellowWheelMotorAxelOffset) 0) $ slamLeft  $ slamFront yellowWheelMotorSlot
        , translate (mk3 ( wheelXOffset) (wheelYOffset - yellowWheelMotorAxelOffset) 0) $ slamRight $ slamFront yellowWheelMotorSlot
        , translate (mk3 0 (wheelYOffset + 13) 0) $ slamFront l298nSlot
        , translate (mk3 0 40 0) $ slamFront ovonicLipoBatterySlot
        , translate (mk3 (-agitatorXOffset) agitatorYOffset 0) agitatorSlot
        , translate (mk3 ( agitatorXOffset) agitatorYOffset 0) agitatorSlot
        , translate (mk3 0 intakeHoleYOffset 0) $ shell baseThickness intakeHoleBB
        , translate (mk3 0 17.5 0) $ slamBack $ slamBottom containerSlot
        ]
    , translate (mk3 (-wheelXOffset) wheelYOffset 0) $ slamRight yellowWheelBB
    , translate (mk3 ( wheelXOffset) wheelYOffset 0) $ slamLeft yellowWheelBB
    , translate (mk3 (-agitatorXOffset) agitatorYOffset 0) agitatorHoleBB
    , translate (mk3 ( agitatorXOffset) agitatorYOffset 0) agitatorHoleBB
    , translate (mk3 0 intakeHoleYOffset (-baseThickness)) intakeHoleBB
    , translate (mk3 0 (intakeHoleYOffset - baseThickness) 0) bagHoleBB
    ]

containerYOffset :: R
containerYOffset = 0

wheelXOffset :: R
wheelXOffset = 50

wheelYOffset :: R
wheelYOffset = -85

agitatorXOffset :: R
agitatorXOffset = 60

agitatorYOffset :: R
agitatorYOffset = 50


agitatorHoleBB :: SymbolicObj3
agitatorHoleBB = centeredBox 15 15 25

agitatorSlot :: SymbolicObj3
agitatorSlot = extrudedSlot 2 2 $ centeredBox 21 21 25

containerBB :: SymbolicObj3
containerBB = centeredBox 118 33 63

container :: SymbolicObj3
container = shell baseThickness containerBB

containerSlot :: SymbolicObj3
containerSlot
  = extrudedSlot thickness 10
  $ expand (mk3 baseThickness baseThickness baseThickness)
  $ containerBB



intakeHoleBB :: SymbolicObj3
intakeHoleBB = slamBack $ slamBottom $ centeredBox 130 15 intakeHoleZSize

intakeHoleYOffset :: R
intakeHoleYOffset = 10 + containerYOffset + 15 + 7

bagHoleXSize :: R
bagHoleXSize = 30

intakeHoleZSize :: R
intakeHoleZSize = 20

bagHoleBB :: SymbolicObj3
bagHoleBB = slamBack $ slamBottom $ centeredBox bagHoleXSize 20 (intakeHoleZSize / 2)

