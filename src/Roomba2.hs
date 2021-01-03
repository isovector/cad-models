{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall        #-}

module Roomba2 where

import Boxes
import Lib
import Data.Foldable
import Graphics.Implicit.Primitives
import StdParts
import Graphics.Implicit.Definitions
import Merge (merging, cut)



main :: IO ()
main = writeSTL 2 "/tmp/roomba2.stl" $
  center3 $ blowerContainer
    -- slamBottom blowerMount2 <> slamTop ( slamBottom bagContainerLid2 <> slamTop (bagContainer2))


finishedBagContainerLid :: IO ()
finishedBagContainerLid = writeSTL 0.5 "/tmp/final-bag-container-lid.stl" $ bagContainerLid2

finishedBagContainer :: IO ()
finishedBagContainer = writeSTL 0.5 "/tmp/final-bag-container.stl" $ bagContainer2

finishedBlowerMount :: IO ()
finishedBlowerMount = writeSTL 0.5 "/tmp/final-blower-mount.stl" $ blowerMount2



plateRoundness :: R
plateRoundness = 45

plate :: SymbolicObj3
plate = freeze $
  difference
    (roundedPlate 5 plateRoundness 200 200 2)
    [translate (mk3 0 0 $ -0.5) plateWalls]

freeze :: SymbolicObj3 -> SymbolicObj3
freeze = intersect . pure

plateWalls :: SymbolicObj3
plateWalls = reflected $
  mconcat
    [ freeze $ slamBottom $ difference
        (roundedPlate 5 plateRoundness (x - th2) (y - th2) z)
        [ roundedPlate 5 plateRoundness (x - th2 * 2) (y - th2 * 2) z
        ]
    , slamBottom $ translate (mk3 (negate $ x / 2 - 6) 0 0.5) $ m3MountingPlate
    ]
  where
    x = 200 - th / 2
    y = 200 - th / 2
    z = 53
    th = 2
    th2 = 2 * th

plateBB :: SymbolicObj3
plateBB = roundedPlate 5 plateRoundness 200 200 200

plateWithWheels :: SymbolicObj3
plateWithWheels =
  intersect
    [ carve $  -- carve must be here, because it doesnt penetrate intersect
        mconcat
          [ plate
          , reflected $ translateXY (-62) (-28) dcGearedMotor
          -- , cut $ translate (mk3 0 1 0) fanSystem
          , thingsToMount
          -- , slamBottom plateWalls
          ]
    , translate (mk3 0 0 $ -2) $ slamBottom plateBB
    ]

thingsToMount :: SymbolicObj3
thingsToMount =
  mconcat []
    -- [ translateXY (-70) 45 l298nSlot
    -- , translateXY (-72) 70 $ slamFront arduinoMiniSlot
    -- , translateXY 0 80 casterMountingHoles
    -- , translateXY 78 92 $ slamBack ovonicLipoBatterySlot
    -- , translate (mk3 0 (-100) 30) hcSr04
    -- ]



------------------------------------------------------------------------------

fanSystem :: SymbolicObj3
fanSystem = slamBottom $ center3 $ carve $
  insetting
    (slamBottom fanContainer)
    [ ( [Abut 60 OnBack]
      , slamBottom bagIntakeSlot
      )
    , ( [Abut 0 OnTop, Flush 2 OnFront]
      , inverse $ centeredBox 120 15 5
      )
    ]

fanMountWithExhaust :: SymbolicObj3
fanMountWithExhaust = center3 $ carve $
  inset
    fanMount
    [Flush 2 OnLeft, Abut 2 OnFront, Flush 4 OnBottom]
    exhaustHole
  where
    exhaustHole = inverse $ cube False $ mk3 58 100 28



fanMount :: SymbolicObj3
fanMount = carve $
  insetting
    fanMountPlatform
    [ ( [Flush 16 OnBack, Flush 16 OnRight, Flush (-2) OnBottom]
      , hole
      )
    , ( [Flush 16 OnFront, Flush 16 OnLeft, Flush (-2) OnBottom]
      , hole
      )
    ]
  where
    hole = inverse $ cylinder 2 10

fanMountPlatform :: SymbolicObj3
fanMountPlatform =
  intersect
    [ container 120 120 0 2
    , rotate3 (degZ 45) $ container 200 20 0 2
    ]



-----------------


blowerContainer :: SymbolicObj3
blowerContainer =
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


blowerMount :: SymbolicObj3
blowerMount = mconcat
  [ -- slamBack $ slamBottom $ blowerContainer
    slamTop $ slamBack $ withoutCubeFaces [OnTop, OnBack, OnBottom] 120 120 35 2
  ]

blowerMount2 :: SymbolicObj3
blowerMount2 = carve $
  mconcat
    [ slamBottom $ slamBack $ blowerMount
    , translate (V3 0 12 9) $ slamBottom $ slamBack $ inverse $ rotate3 (degY 90) $ center3 $ cylinder 12 500
    ]

bagContainer2 :: SymbolicObj3
bagContainer2 =
  intersect
    [ merging (slamBottom $ insideContainer 119.8 119.8 32.9 2)
      [ translate (V3 0 (-20) (-2)) $ slamBottom $ withoutCubeFaces [OnTop, OnBottom] 80 15 6 1
      ]
    , slamBottom $ centeredBox 119.8 119.8 32.9
    ]


bagContainerLid2 :: SymbolicObj3
bagContainerLid2 =
  difference
    ( mconcat
        [ slamTop $ mirror (V3 0 0 1) $ insideContainer (117.7 - 0.8 * 2) (117.7 - 0.8 * 2) 5 1.2
        , slamTop $ atSamePlace centeredAirHole
            $ reflected
            $ translateXY 40 0
            $ withoutCubeFaces [OnTop, OnLeft, OnFront] 10 80 10 1.2
        , slamTop $ insideContainer 119.8 119.8 0 1.2
        ]
    )
    [ centeredAirHole
    ]



-----------------



intakeTube :: SymbolicObj3
intakeTube = mconcat
  [ slamBottom $
      mconcat
        [ slamTop $ barrel (84/2) 35 2
        , slamTop $ cylinder (96/2) 2
        ]
    -- BUG: these are 2mm inside of the tube!!
  -- , reflected $ translateXY 42 0 m3MountingPlate
  -- , rotate3 (degZ 90) $ translateXY 42 0 m3MountingPlate
  ]



fanContainer :: SymbolicObj3
fanContainer = carve $
  inset
    fanMountWithExhaust
    [Abut 2 OnTop, Flush 8 OnRight, Flush (22-8) OnFront]
    intakeTube

intakeBit :: SymbolicObj3
intakeBit =
  mconcat
    [ slamBottom $ rotate3 (degX 180) $ container 120 15 15 2
    , reflected $ translateXY 64 0 m3MountingPlate
    ]

bagIntakeSlot :: SymbolicObj3
bagIntakeSlot =
  inset intakeBit [Abut 2 OnFront, Flush 0 OnBottom] bagHolderWithAlignment

bagHolder :: SymbolicObj3
bagHolder = carve $ mconcat
  [ slamBottom $ mkBagVentilationHole 30 $ container 100 50 15 2
  , reflected $ translateXY 54 (-5) m3MountingPlate
  , reflected $ translateXY 54 20 m3MountingPlate
  ]

mkBagVentilationHole :: R -> SymbolicObj3 -> SymbolicObj3
mkBagVentilationHole w s =
    insetting
      s
      [ ( [Abut 2 OnBack, Flush 2 OnBottom]
        , inverse $ cube True $ mk3 w 15 10
        )
      ]

bagHolderWithAlignment :: SymbolicObj3
bagHolderWithAlignment =
  insetting
    bagHolder
    [ --( [Flush 0 OnLeft, Flush 0 OnBack, Flush 2 OnBottom]
      -- TODO(sandy): fix this carve
      -- , cube True (10, 10, 50)
      -- )
      ( [Abut 2 OnFront, Flush 0 OnBottom]
      , wedgeThing 48 60 13 17 2
      )
    ]


wedgeThing
  :: R -- ^ half width
  -> R -- ^ y
  -> R -- ^ hole z
  -> R -- ^ total z
  -> R -- ^ thickness
  -> SymbolicObj3
wedgeThing x y hz tz th =
  difference
    (flip extrude tz $ polygon [(mk2 (-x) 0), (mk2 x 0), (mk2 0 y)])
    [ translate (mk3 0 0 th) $
        flip extrude hz $ polygon [mk2 (-x + th) 0, mk2 (x - th) 0, mk2 0 (y - th)]
    ]


------------------------------------------------------------------------------


dcWheelWithWell :: SymbolicObj3
dcWheelWithWell = rotate3 (degY 90) $
  let wheel = cylinder 35 28
      wheelBB = translate (mk3 11 0 14) $ centeredBox 2 74 28
      -- TODO(sandy): temporary for making the base plate
      fender = const mempty $
        intersect
          [ shell 2 $ outset 5 wheel
          , cylinder 50 28
          , translate (mk3 11 0 0) $ slamRight $ slamBottom $ cube True (mk3 100 100 28)
          ]
   in flush fender 0 OnBottom $ inverse $ wheelBB <> outset 2.5 wheel

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


hcSr04 :: SymbolicObj3
hcSr04 = inverse $ center3 $ rotate3 (degX 90) $ reflected $ translateXY 18 0 $ cylinder 8.5 11

hcSr04TempHoles :: SymbolicObj3
hcSr04TempHoles = inverse $ center3 $ rotate3 (degX $ -90) $ reflected $ translateXY 12.5 0 $
  inset (cylinder 8.1 11)
    [Flush 0 OnBottom, Abut (8.1) OnBack]
    $ center3 $ box (8.1 * 2) (8.1*2) 11

hcSr04Temp :: SymbolicObj3
hcSr04Temp = carve $
  mconcat
    [ slamBottom $ inset (container 46.5 8 21 1)
        [Abut 2 OnFront, Flush 4 OnBottom]
        hcSr04TempHoles
    , reflected $ translateXY 22 (-4) $ slamBottom $ slamLeft m3MountingPlate
    ]



------------------------------------------------------------------------------

casterMountingHoles :: SymbolicObj3
casterMountingHoles =
  inverse $ center3 $ reflected $ translateXY (38/2) 0 $ cylinder (3.4 / 2) 9


------------------------------------------------------------------------------

filterTapHolder :: SymbolicObj3
filterTapHolder =
  reflected $ translateXY (48 - 0.5 / 2) 0 $ slamRight $ centeredBox 5 2 20

filterTap :: SymbolicObj3
filterTap = mirrored (V3 0 0 1) $ slamTop $ mconcat $
  [ slamFront $ slamTop $ centeredBox 95.8 4 2
  , translate (V3 0 4 (2)) $ slamFront  $ slamTop $ centeredBox 95.8 1 4
  , slamBottom $ slamFront $ center3 $ mconcat $ do
      i <- [0..8]
      pure $
        slamFront $
        translateXY (-(95.8 / 2) + fromIntegral @Int i * (95.8 / 8 - 2)) 0 $
          centeredBox 4 1.5 8
  ]


heldFilterTap :: SymbolicObj3
heldFilterTap =
  difference (slamBack filterTap)
    [ slamBack filterTapHolder
    ]

filterCloser :: SymbolicObj3
filterCloser =
  difference (centeredBox 86 1.75 15.5)
    [ centeredBox 82 1.75 12
    ]


vacuumContainer :: SymbolicObj3
vacuumContainer
  = inset filterTapHolder [Flush (-1.6) OnBottom, Flush (-5) OnFront]
  $ freeze
  $ carve
  $ mkBagVentilationHole 90
  $ rotate3 (degZ 180)
  $ mkBagVentilationHole 30
  $ container (100 - 4 - 0.5) (50 - 4 - 0.5) (64 + topClearance - 2 - 2) 1.6

vacuumTop :: SymbolicObj3
vacuumTop = rotate3 (degX 180) $ mconcat
  [ slamBottom $ container (100 - 0.4) (50 - 0.4) 8 2
  , slamBottom $ container 104 54 2 2
  ]

topClearance :: R
topClearance = 10

buckConverterStack :: SymbolicObj3
buckConverterStack = mconcat
  [ slamBottom $ difference
      (slamTop $ center3 $ container 44 21 18 2)
      [slamTop $ centeredBox 88 (21 + 0.5) 18]
  , slamTop stackSeps
  ]


stackSeps :: SymbolicObj3
stackSeps = mconcat
  [ translateXY (43.3 / 2 - 5.75) (21 / 2 - 2) $ cylinder 1.4 2
  , mirror (V3 0 1 0) $ mirror (V3 1 0 0) $ translateXY (43.3 / 2 - 5.75) (21 / 2 - 2) $ cylinder 1.4 2
  ]

arduinoTemp :: SymbolicObj3
arduinoTemp =
  slamBottom $ center3 $ carve $
    mconcat
      [ slamBottom $ slamLeft $ container 45 18.5 4.5 1.5
        -- usb port
      , inverse $ translate (mk3 (-5) 0 1.5) $ slamBottom $ slamLeft $ centeredBox 10 8 4.4
      ]

