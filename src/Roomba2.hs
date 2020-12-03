{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall        #-}

module Roomba2 where

import Lib
import Data.Foldable
import Graphics.Implicit.Primitives
import StdParts
import Merge


main :: IO ()
main = writeSTL 0.75 "/tmp/roomba2.stl" $
  center3 $ plateWithWheels


plate :: SymbolicObj3
plate = roundedPlate 5 50 200 200 2.5

plateWalls :: SymbolicObj3
plateWalls =
  difference
    (roundedPlate 5 50 200 200 53)
    [ roundedPlate 5 50 196 196 53 ]

plateBB :: SymbolicObj3
plateBB = roundedPlate 5 50 200 200 200


plateWithWheels :: SymbolicObj3
plateWithWheels =
  intersect
    [ merge (slamBottom plateWalls) $
        carve $  -- carve must be here, because it doesnt penetrate intersect
          let reflected x = x <> mirror (1, 0, 0) x
          in mconcat
                [ plate
                , reflected $ translateXY (-58) 23 $ dcGearedMotor
                , translateXY 0 3 $ rotate3 (degZ 180) fanSystem
                , thingsToMount
                ]
    , translate (0, 0, -2.5) $ slamBottom plateBB
    ]

thingsToMount :: SymbolicObj3
thingsToMount =
  mconcat
    [ translateXY (-70) (-45) l298nSlot
    , translateXY (0) (-85) arduinoMiniSlot
    , translateXY (57) (-50) ovonicLipoBatterySlot
    ]



------------------------------------------------------------------------------

fanSystem :: SymbolicObj3
fanSystem = translate (0, 0, -5) $ slamBottom $ center3 $
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
fanMountWithExhaust = carve $
  inset
    fanMount
    [Flush 2 OnLeft, Abut 2 OnFront, Flush 4 OnBottom]
    exhaustHole
  where
    exhaustHole = inverse $ cubeR 0 False (58, 100, 28)

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
intakeTube = barrel 40 30 2



fanContainer :: SymbolicObj3
fanContainer = carve $
  inset
    fanMountWithExhaust
    [Abut 2 OnTop, Flush 16 OnRight, Flush 22 OnFront]
    intakeTube

intakeBit :: SymbolicObj3
intakeBit = rotate3 (degX 180) $ container 120 15 15 2

bagIntakeSlot :: SymbolicObj3
bagIntakeSlot =
  inset intakeBit [Abut 2 OnFront, Flush 0 OnBottom] bagHolderWithAlignment

bagHolder :: SymbolicObj3
bagHolder = carve $
  insetting
    (container 100 50 15 2)
    [ ( [Abut 2 OnBack, Flush 2 OnBottom]
      , inverse $ cubeR 0 True (30, 15, 10)
      )
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
    [ cubeR 0 True (x + 2 * th, y + 2 * th, z + th)
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
    (extrude tz $ polygonR 0 [(-x, 0), (x, 0), (0, y)])
    [ translate (0, 0, th) $
        extrude hz $ polygonR 0 [(-x + th, 0), (x - th, 0), (0, y - th)]
    ]


------------------------------------------------------------------------------


dcWheelWithFender :: SymbolicObj3
dcWheelWithFender = rotate3 (degY 90) $
  let wheel = cylinder 35 28
      wheelBB = translate (11, 0, 14) $ centeredBox 2 74 28
      fender =
        intersect
          [ shell 2 $ outset 5 wheel
          , cylinder 50 28
          , translate (11, 0, 0) $ slamRight $ slamBottom $ cubeR 0 True (100, 100, 28)
          ]
   in flush fender 0 OnBottom $ abut (inverse $ cylinder 3 6) 0 OnTop $ inverse $ wheelBB <> wheel

dcGearedMotor :: SymbolicObj3
dcGearedMotor = slamRight $ translate (0, 0, negate $ 35 - 11) $ slamBottom $
  let motor = cubeR 0 True (19, 22, 65)
   in inset
        dcWheelWithFender
        [Abut 2 OnLeft, Flush (35 - 11 - 5.5) OnBottom]
        $ mconcat
            [ inverse $ slamBottom motor
            , extrudedSlot 2 2 motor
            , inverse $ slamTop $ centeredBox 3 5.5 5.5
            ]

