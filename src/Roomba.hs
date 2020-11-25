module Roomba where

import Lib
import StdParts
import Merge (merging)


main :: IO ()
main = do
  writeSTL 0.5 "/tmp/roomba.stl" $ do
    funnel

roombaBottom :: SymbolicObj3
roombaBottom =
    intersect
      [ basePlate
      , center3 $ expand (0, 0, 100) stockPlate
      ]

shellSlot :: SymbolicObj3
shellSlot =
  expand (mk3 (-shellInset) (-shellInset) 0) $
      difference
        [ shell 2.5 $ stockPlate
        , center3 $ expand (mk3 0 0 10) stockPlate
        ]

funnel :: SymbolicObj3
funnel =
  let pyr   = slamBottom $ center3 $ pyramid 5 120 120 30
   in difference
        [ pyr
        , scale (mk3 0.95 0.95 0.95) pyr
        , cylinder 6 40
        ]


------------------------------------------------------------------------------

shellInset :: R
shellInset = 10


baseThickness :: R
baseThickness = 2.5

thickness :: R
thickness = 1.5


plateSize :: R
plateSize = 180


stockPlate :: SymbolicObj3
stockPlate
  = slamTop
  $ difference
    [ center3
      $ extrude baseThickness
      $ union
      $ let half = plateSize / 2
        in [ slamBack $ rectR half zero $ mk2 plateSize plateSize
           , slamBack $ rectR 5 zero $ mk2 plateSize half
           ]
    , translate (mk3 (-wheelXOffset) wheelYOffset 0) $ slamRight yellowWheelBB
    , translate (mk3 ( wheelXOffset) wheelYOffset 0) $ slamLeft yellowWheelBB
    ]

basePlate :: SymbolicObj3
basePlate =
  merging
    ( union
        [ stockPlate
        , translate (mk3 0 (wheelYOffset + 8) 0) $ slamFront l298nSlot
        , translate (mk3 0 65 0) $ ovonicLipoBatterySlot
        , translate (mk3 (-60) 70 0) $ arduinoMiniSlot
        , slamBottom shellSlot
        ]
    )
    [ translateXY (-agitatorXOffset) agitatorYOffset agitatorM
    , translateXY   agitatorXOffset  agitatorYOffset agitatorM
    , translateXY (-wheelXOffset) motorYOffset $ slamLeft motorM
    , translateXY ( wheelXOffset) motorYOffset $ slamRight motorM
    , translateXY 0 intakeHoleYOffset intakeM
    ]
  where
    motorYOffset = wheelYOffset - yellowWheelMotorAxelOffset


motorM :: SymbolicObj3
motorM =
  slamFront $ difference
    [ yellowWheelMotorSlot
    , slamBottom yellowWheelMotorBB
    ]


flipSwitchBB :: SymbolicObj3
flipSwitchBB = difference
  [ mempty
  , rotate3 (degY 90) $ cylinder 3.3 10
  ]


intakeM :: SymbolicObj3
intakeM = difference
  [ slamBack $ union
      [ slamFront $ shell baseThickness intakeHoleBB
      , translateXY 0 baseThickness $ slamBack containerSlot
      ]
  , translate
      (mk3 0 (negate $ baseThickness / 2) (-baseThickness)) $
        slamBack intakeHoleBB
  , translateXY 0 (-baseThickness) bagHoleBB
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


agitatorM :: SymbolicObj3
agitatorM = difference
  [ agitatorSlot
  , agitatorHoleBB
  ]




agitatorHoleBB :: SymbolicObj3
agitatorHoleBB = centeredBox 6.5 6.5 25

agitatorSlot :: SymbolicObj3
agitatorSlot = extrudedSlot 2 2 $ centeredBox 20 15 25

containerBB :: SymbolicObj3
containerBB = centeredBox 118 30 63

container :: SymbolicObj3
container = shell baseThickness containerBB

containerSlot :: SymbolicObj3
containerSlot
  = extrudedSlot thickness 10
  $ expand (mk3
      (baseThickness * 2)
      (baseThickness * 2)
      (baseThickness * 2))
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
bagHoleBB
  = translateXY 0 (-10)
  $ slamBack
  $ slamBottom
  $ centeredBox bagHoleXSize 10
  $ intakeHoleZSize / 2

