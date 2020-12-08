{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall        #-}

module Roomba2 where

import Lib
import Data.Foldable
import Graphics.Implicit.Primitives
import StdParts
import Graphics.Implicit.Definitions
import Simplify


testSlice :: Alignment -> R -> SymbolicObj3 -> SymbolicObj3
testSlice a th s =
  intersect
    [ slam a $ cubeR 0 True $ mk3 500 500 th
    , slam a s
    ]


main :: IO ()
main = writeSTL 0.5 "/tmp/roomba2.stl" $
  center3 fanSystem

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

reflected :: SymbolicObj3 -> SymbolicObj3
reflected x = x <> mirror (mk3 1 0 0) x

plateWithWheels :: SymbolicObj3
plateWithWheels =
  intersect
    [ carve $  -- carve must be here, because it doesnt penetrate intersect
        mconcat
          [ plate
          , reflected $ translateXY (-58) (-28) $ dcGearedMotor
          , translate (mk3 0 1 0) fanSystem
          , thingsToMount
          , slamBottom plateWalls
          ]
    , translate (mk3 0 0 $ -2) $ slamBottom plateBB
    ]

thingsToMount :: SymbolicObj3
thingsToMount =
  mconcat
    [ translateXY (-70) 45 l298nSlot
    , translateXY (-72) 70 $ slamFront arduinoMiniSlot
    , translateXY 0 80 casterMountingHoles
    , translateXY 78 55 ovonicLipoBatterySlot
    , translate (mk3 0 (-100) 30) hcSr04
    ]



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
    exhaustHole = inverse $ cubeR 0 False $ mk3 58 100 28

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

insetting
    :: Foldable t
    => SymbolicObj3
    -> t ([Inset], SymbolicObj3)
    -> SymbolicObj3
insetting = foldl' (uncurry . inset)

intakeTube :: SymbolicObj3
intakeTube = mconcat
  [ slamBottom $
      mconcat
        [ slamTop $ barrel (84/2) 35 2
        , slamTop $ cylinder (96/2) 2
        ]
    -- BUG: these are 2mm inside of the tube!!
  , reflected $ translateXY 42 0 m3MountingPlate
  , rotate3 (degZ 90) $ translateXY 42 0 m3MountingPlate
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
  [ slamBottom $ insetting
      (container 100 50 15 2)
      [ ( [Abut 2 OnBack, Flush 2 OnBottom]
        , inverse $ cubeR 0 True $ mk3 30 15 10
        )
      ]
  , reflected $ translateXY 54 (-5) m3MountingPlate
  , reflected $ translateXY 54 20 m3MountingPlate
  ]

bagHolderWithAlignment :: SymbolicObj3
bagHolderWithAlignment =
  insetting
    bagHolder
    [ --( [Flush 0 OnLeft, Flush 0 OnBack, Flush 2 OnBottom]
      -- TODO(sandy): fix this carve
      -- , cubeR 0 True (10, 10, 50)
      -- )
      ( [Abut 2 OnFront, Flush 0 OnBottom]
      , wedgeThing 48 60 13 17 2
      )
    ]



roundedPlate
    :: R  -- ^ rounding on "flat" side
    -> R  -- ^ rounding on "rounded" side
    -> R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ thickness
    -> SymbolicObj3
roundedPlate r1 r2 x y z = slamTop $
  center3
    $ extrude z
    $ union
    $ let half = y / 2
       in [ slamBack $ rectR r2 zero $ mk2 x y
          , slamBack $ rectR r1 zero $ mk2 x half
          ]


container
    :: R -- ^ w
    -> R -- ^ d
    -> R -- ^ h
    -> R -- ^ th
    -> SymbolicObj3
container x y z th = carve $
  allFlush OnTop
    [ cubeR 0 True $ mk3 (x + 2 * th) (y + 2 * th) (z + th)
    , inverse $ centeredBox x y z
    ]


barrel
    :: R  -- ^ radius
    -> R  -- ^ height
    -> R  -- ^ thickness
    -> SymbolicObj3
barrel r h th = carve $
  allFlush OnTop
    [ cylinder r h
    , inverse $ cylinder (r - th) (h - th)
    ]

tube
    :: R  -- ^ radius
    -> R  -- ^ height
    -> R  -- ^ thickness
    -> SymbolicObj3
tube r h th =
  difference
      (cylinder r h)
      [ cylinder (r - th) h ]


wedgeThing
  :: R -- ^ half width
  -> R -- ^ y
  -> R -- ^ hole z
  -> R -- ^ total z
  -> R -- ^ thickness
  -> SymbolicObj3
wedgeThing x y hz tz th =
  difference
    (extrude tz $ polygonR 0 [(mk2 (-x) 0), (mk2 x 0), (mk2 0 y)])
    [ translate (mk3 0 0 th) $
        extrude hz $ polygonR 0 [mk2 (-x + th) 0, mk2 (x - th) 0, mk2 0 (y - th)]
    ]


------------------------------------------------------------------------------


dcWheelWithWell :: SymbolicObj3
dcWheelWithWell = rotate3 (degY 90) $
  let wheel = cylinder 35 28
      wheelBB = translate (mk3 11 0 14) $ centeredBox 2 74 28
      fender =
        intersect
          [ shell 2 $ outset 5 wheel
          , cylinder 50 28
          , translate (mk3 11 0 0) $ slamRight $ slamBottom $ cubeR 0 True (mk3 100 100 28)
          ]
   in flush fender 0 OnBottom $ abut (inverse $ cylinder 3 6) 0 OnTop $ inverse $ wheelBB <> outset 2.5 wheel

dcGearedMotor :: SymbolicObj3
dcGearedMotor = slamRight $ translate (mk3 0 0 $ negate $ 35 - 11) $ slamBottom $
  let motor = cubeR 0 True $ mk3 19 22 65
   in inset
        dcWheelWithWell
        [Abut 2 OnLeft, Flush (35 - 11 - 5.5) OnBottom]
        $ mconcat
            [ inverse $ slamBottom motor
            , extrudedSlot 2 6.5 motor
            , inverse $ slamTop $ centeredBox 3 5.5 5.5
            ]


hcSr04 :: SymbolicObj3
hcSr04 = inverse $ center3 $ rotate3 (degX 90) $ reflected $ translateXY 18 0 $ cylinder 8.5 11

------------------------------------------------------------------------------

casterMountingHoles :: SymbolicObj3
casterMountingHoles =
  inverse $ center3 $ reflected $ translateXY (38/2) 0 $ cylinder (3.4 / 2) 9



