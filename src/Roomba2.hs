{-# LANGUAGE TypeApplications #-}

module Roomba2 where

import Lib


main :: IO ()
main = writeSTL 2 "/tmp/roomba2.stl" $
  center3 $ fanCountainer


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
  inset (
    inset
      (container 120 120 32 2)
      [Flush 16 OnBack, Flush 16 OnRight, Flush (-2) OnBottom]
      hole)
    [Flush 16 OnFront, Flush 16 OnLeft, Flush (-2) OnBottom]
    hole
  where
    hole = inverse $ cylinder 2 10

intakeTube :: SymbolicObj3
intakeTube = barrel 40 30 2

fanCountainer :: SymbolicObj3
fanCountainer = carve $
  inset fanMountWithExhaust [Abut 2 OnTop, Flush 16 OnRight, Flush 22 OnFront] intakeTube



roundedPlate
    :: R  -- ^ rounding on "curved" side
    -> R  -- ^ rounding on "flat" side
    -> R  -- ^ width
    -> R  -- ^ depth
    -> R  -- ^ thickness
    -> SymbolicObj3
roundedPlate r1 r2 x y z =
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

