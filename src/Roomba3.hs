module Roomba3 where

import GHC.Generics
import Linear hiding (R2)
import Boxes
import Lib
import StdParts
import Merge (cut, merging)
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives



main :: IO ()
main = writeSTL 0.5 "/tmp/roomba2.stl" $
  center3 $ tap

translateXYV :: R2 -> SymbolicObj3 -> SymbolicObj3
translateXYV (V2 x y) = translateXY x y

final_plate :: SymbolicObj3
final_plate = flip difference [translate (V3 0 0 (-wall_groove)) plateWalls] $
  carve $ mconcat
  [ plate
  , inverse $ translate (0 & _xy .~ hole_pos) $ slamBack $ centeredBox  80 15 6
  , reflected $ translate (0 & _xy .~ agi_pos) agitatorSlot
  , reflected $ translate (0 & _xy .~ wheel_pos) dcGearedMotor
  , translate (0 & _xy .~ arduino_pos) $ slamFront $ rotate3 (degZ 90) arduinoMiniSlot
  , translate (0 & _xy .~ controller_pos) $ slamBack l298nSlot
  , translate (0 & _xy .~ battery_pos) $ placeholder $ slamBack ovonicLipoBatterySlot
  , translate (0 & _xy .~ buck_pos) $ slamBack $ rotate3 (degZ 90) $ placeholder buckConverterStack
  , translate (0 & _xy .~ caster_pos) $ casterMountingHoles
  ]
  where
    back_align = -35
    hole_pos = V2 0 (100 - 72.75)
    wall_groove = 0.5

    agi_pos = V2 78 68
    wheel_pos = V2 (-70) 10
    controller_pos = V2 (-60) back_align
    battery_pos = V2 0 back_align
    arduino_pos = V2 (80) (-105)
    buck_pos = V2 45 back_align
    caster_pos = V2 0 (-100)


final_lid :: SymbolicObj3
final_lid = bagContainerLid2


bagContainer2 :: SymbolicObj3
bagContainer2 =
  mconcat
    [ merging (slamBottom $ insideContainer 119.8 119.8 32.9 2)
      [ translate (V3 0 (-20) (-2)) $ slamBottom $ withoutCubeFaces [OnTop, OnBottom] 80 15 6 1
      ]
    , slamBottom $ centeredBox 119.8 119.8 32.9
    ]


agitatorSlot :: SymbolicObj3
agitatorSlot = mconcat
  [ extrudedSlot 2 4 $ slamBottom $ cube True $ V3 w d h
  , inverse $ slamTop $ cylinder hole_rad hole_depth
  ]
  where
    w = 14.97
    d = 20
    h = 10 -- 25.01
    hole_rad = 5.95 / 2
    hole_depth = 10

plate :: SymbolicObj3
plate = slamTop $ mirror (V3 0 1 0) $ roundedPlate front_round back_round w d th
  where
    w = 200
    d = 200
    th = 2
    front_round = 5
    back_round = 45

plateWalls :: SymbolicObj3
plateWalls = Shared $ IntersectR 0 $ pure $ slamBottom $ mirror (V3 0 1 0) $
  difference (roundedPlate front_round back_round (w - 2 * th) (d - 2 * th) h) $
    pure $
      roundedPlate front_round back_round (w - 4 * th) (d - 4 * th) h

  where
    w = 200
    d = 200
    h = 80
    th = 2
    front_round = 5
    back_round = 45



placeholder_vacuumContainer :: SymbolicObj3
placeholder_vacuumContainer = placeholder $ slamBottom $ cube True $ V3 w d h
  where
    w = 123.7
    d = 123.7
    h = 10 -- 74.5

dcWheelWithWell :: SymbolicObj3
dcWheelWithWell = rotate3 (degY 90) $
  let wheel = cylinder wheel_rad wheel_depth
      wheelBB = translate (mk3 11 0 14) $ centeredBox 2 ((wheel_rad + bb_tolerance) * 2) wheel_depth
      -- TODO(sandy): temporary for making the base plate
      fender = const mempty $
        intersect
          [ shell 2 $ outset 5 wheel
          , cylinder 50 28
          , translate (mk3 11 0 0) $ slamRight $ slamBottom $ cube True (mk3 100 100 28)
          ]
   in flush fender 0 OnBottom $ inverse $ wheelBB <> outset 2.5 wheel
  where
    bb_tolerance = 2
    wheel_rad = 35
    wheel_depth =  28

dcGearedMotor :: SymbolicObj3
dcGearedMotor = slamRight $ translate (mk3 0 0 $ negate $ 35 - 11) $ slamBottom $
  let motor = cube True $ mk3 19 22 65
   in inset
        dcWheelWithWell
        [Abut 1.5 OnLeft, Flush (35 - 11 - 5.5) OnBottom]
        $ mconcat
            [ inverse $ slamBottom motor
            , extrudedSlot 2 6.5 motor
            , inverse $ slamTop $ centeredBox 3 5.5 5.5
            ]

placeholder :: SymbolicObj3 -> SymbolicObj3
placeholder = const mempty

buckConverterStack :: SymbolicObj3
buckConverterStack = mconcat
  [ slamBottom $ difference
      (slamTop $ center3 $ container 44 21 18 2)
      [slamTop $ centeredBox 88 (21 + 0.5) 18]
  , slamTop stackSeps
  ]
  where
    stackSeps = mconcat
      [ translateXY (43.3 / 2 - 5.75) (21 / 2 - 2) $ cylinder 1.4 2
      , mirror (V3 0 1 0) $
          mirror (V3 1 0 0) $
            translateXY (43.3 / 2 - 5.75) (21 / 2 - 2) $
              cylinder 1.4 2
      ]


casterMountingHoles :: SymbolicObj3
casterMountingHoles
  = inverse
  $ translateXY 0 (32 / 2)
  $ slamFront
  $ center3
  $ reflected
  $ translateXY (38/2) 0
  $ cylinder (3.4 / 2) 9


final_blowerContainer :: SymbolicObj3
final_blowerContainer =
  -- inlined from other combinators because I couldn't figure out the math
   difference
    (translate (V3 (-62.0) (-62.0) 7.5) $ cube False $ V3 124.0 124.0 18.0)
    [ union
      [ translate (V3 (-60.0) (-60.0) 9.5) $ cube False $ V3 120.0 120.0 16.0
      , translate (V3 (-60.0) 60.0 11.5) $ cube False $ V3 58.0 100.0 28.0
      , centeredAirHole
      ]
    ]


centeredAirHole :: SymbolicObj3
centeredAirHole = translate (V3 6 0 (-23.5)) (cylinder 40.0 50.0)

bagContainerLid2 :: SymbolicObj3
bagContainerLid2 =
  difference
    ( mconcat
        [ slamTop $ atSamePlace centeredAirHole
            $ reflected
            $ translateXY 40 0
            $ withoutCubeFaces [OnTop, OnLeft, OnFront] tap_holder_w 80 (tap_height + th) th
        , slamTop $ insideContainer 119.8 119.8 0 th
        , slamTop $ mirror (V3 0 0 1) $ insideContainer inside_size inside_size (th + tap_height) th
        ]
    )
    [ centeredAirHole
    , translate (V3 0 (-20) (-th)) $ slamTop $ atSamePlace centeredAirHole $ centeredBox ((33 + tap_holder_w) * 2) 80 (tap_height + th)
    ]
  where
    inside_size = 113
    tap_height = 3
    tap_holder_w = 10
    th = 1.2



final_blowerMount :: SymbolicObj3
final_blowerMount = carve $
  mconcat
    [ slamBottom $ slamBack $ withoutCubeFaces [OnTop, OnBack, OnBottom] 120 120 35 2
    , translate (V3 0 12 9) $ slamBottom $ slamBack $ inverse $ rotate3 (degY 90) $ center3 $ cylinder 12 500
    ]


final_agitator :: SymbolicObj3
final_agitator = carve $ mconcat $
  [ cylinder rad th
  , inverse $ cylinder axel_rad th
  ] <> do
    let brush_hole = translateXY 0 (rad + brush_rad)
                   $ rotate3 (degX 45)
                   $ center3
                   $ cylinder brush_rad
                   $ th * 12
    ix <- [0 .. brushes - 1]
    pure $ inverse $ rotate3 (degZ $ 360 / brushes * ix) brush_hole
  where
    rad = 10
    th = 2
    axel_rad = 2.3 / 2
    brush_rad = 1
    brushes = 8


tap :: SymbolicObj3
tap = mconcat $
  [ slamBottom $ carve $ mconcat
    [ slamTop $ centeredBox w d th
    , inverse $ slamTop $ centeredBox (w - lip) (d - lip) half_th
    , inverse $ slamTop $ centeredBox (w - lip * 3) (d - lip * 3) th
    ]
  , slamBottom $ centeredBox 5 d half_th
  , slamBottom $ centeredBox w 5 half_th
  ]
  where
    w = 89
    d = 80
    th = 3
    lip = 4

    half_th = th / 2

tapHolder :: SymbolicObj3
tapHolder = mconcat $
  [ slamBottom $ carve $ mconcat
    [ slamTop $ centeredBox (w - lip) (d - lip) half_th
    , inverse $ slamTop $ centeredBox (w - lip * 3) (d - lip * 3) th
    ]
  , slamBottom $ centeredBox 5 (d - lip) half_th
  , slamBottom $ centeredBox (w - lip) 5 half_th
  ]
  where
    w = 89
    d = 80
    th = 3
    lip = 4

    half_th = th / 2



