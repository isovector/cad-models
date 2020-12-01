module Roomba where

import Lib
import StdParts
import Merge (merging)
import Roomba.Intake (suckHole)


main :: IO ()
main = do
  writeSTL 2 "/tmp/roomba.stl" $ do
    roombaBottom

roombaBottom :: SymbolicObj3
roombaBottom =
    intersect
      [ basePlate
      , center3 $ expand (0, 0, 100) stockPlate
      ]


------------------------------------------------------------------------------

shellInset :: R
shellInset = 10


baseThickness :: R
baseThickness = 2.5


plateWidth :: R
plateWidth = 200

plateDepth :: R
plateDepth = 200


stockPlate :: SymbolicObj3
stockPlate
  = slamTop
  $ difference
    ( center3
      $ extrude baseThickness
      $ union
      $ let half = plateDepth / 2
        in [ slamBack $ rectR 50 zero $ mk2 plateWidth plateDepth
           , slamBack $ rectR 5 zero $ mk2 plateWidth half
           ])
    [ translate (mk3 (-wheelXOffset - 4) wheelYOffset 0) $ slamRight yellowWheelBB
    , translate (mk3 ( wheelXOffset + 4) wheelYOffset 0) $ slamLeft yellowWheelBB
    ]

basePlate :: SymbolicObj3
basePlate =
  merging
    ( union
        [ stockPlate
        , translateXY   87  (-35) $ slamRight $ slamBack $ l298nSlot
        , translateXY (-85) (-35) $ slamBack $ slamLeft $ ovonicLipoBatterySlot
        , translateXY 0 (-80) arduinoMiniSlot
        ]
    )
    [
      translateXY (-wheelXOffset) motorYOffset $ slamLeft motorM
    , translateXY ( wheelXOffset) motorYOffset $ slamRight motorM
    , translateXY 0 (-25) $ rotate3 (degZ 180) suckHole
    ]
  where
    motorYOffset = wheelYOffset + yellowWheelMotorAxelOffset


motorM :: SymbolicObj3
motorM =
  slamBack $ difference
    yellowWheelMotorSlot
    [ slamBottom yellowWheelMotorBB
    ]


flipSwitchBB :: SymbolicObj3
flipSwitchBB = difference mempty
  [ rotate3 (degY 90) $ cylinder 3.3 10
  ]


containerYOffset :: R
containerYOffset = 0

wheelXOffset :: R
wheelXOffset = 65

wheelYOffset :: R
wheelYOffset = 10

