{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall        #-}

module Roomba2 where

import Lib
import Data.Foldable
import Graphics.Implicit.Primitives
import Graphics.Implicit.Definitions


main :: IO ()
main = writeSTL 2 "/tmp/roomba2.stl" $
  center3 $ eliminateEmpty $ carve $ dcGearedMotor <> cubeR 0 True (100, 100, 2)

fanSystem :: SymbolicObj3
fanSystem = carve $
  insetting
    (slamBottom fanContainer)
    [ ( [Abut 60 OnBack]
      , slamBottom bagHolderWithAlignment
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
    (container 120 120 0 2)
    [ ( [Flush 16 OnBack, Flush 16 OnRight, Flush (-2) OnBottom]
      , hole
      )
    , ( [Flush 16 OnFront, Flush 16 OnLeft, Flush (-2) OnBottom]
      , hole
      )
    ]
  where
    hole = inverse $ cylinder 2 10

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

bagHolder :: SymbolicObj3
bagHolder = carve $
  insetting
    (container 120 50 28 2)
    [ ( [Abut 2 OnBack, Flush 2 OnBottom]
      , inverse $ cubeR 0 True (30, 15, 10)
      )
    ]

bagHolderWithAlignment :: SymbolicObj3
bagHolderWithAlignment =
  insetting
    bagHolder
    [ ( [Flush 0 OnLeft, Flush 0 OnBack, Flush 2 OnBottom]
      -- TODO(sandy): fix this carve
      , cubeR 0 True (10, 10, 50)
      )
    , ( [Abut 2 OnFront, Flush 0 OnBottom]
      , wedgeThing 48 60 15 30 2
      )
    ]



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
        extrude hz $ polygonR 0 [(-x + th, 0), (x - th, 0), (0, x - th)]
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
dcGearedMotor = translate (0, 0, negate $ 35 - 11) $ slamBottom $
  let motor = cubeR 0 True (19, 22, 65)
   in inset
        dcWheelWithFender
        [Abut 2 OnLeft, Flush (35 - 11 - 5.5) OnBottom]
        $ mconcat
            [ inverse $ slamBottom motor
            , extrudedSlot 2 2 motor
            , inverse $ slamTop $ centeredBox 3 5.5 5.5
            ]


------------------------------------------------------------------------------

eliminateEmpty :: SymbolicObj3 -> SymbolicObj3
eliminateEmpty = head . drop 10 . iterate filterEmpty

filterEmpty :: SymbolicObj3 -> SymbolicObj3
filterEmpty Empty3 = Empty3
filterEmpty (Translate3 _ Empty3) = Empty3
filterEmpty (Scale3 _ Empty3) = Empty3
filterEmpty (Rotate3 _ Empty3) = Empty3
filterEmpty (Mirror3 _ Empty3) = Empty3
filterEmpty (Shell3 _ Empty3) = Empty3
filterEmpty (Outset3 _ Empty3) = Empty3
filterEmpty (Translate3 r s) = Translate3 r $ filterEmpty s
filterEmpty (Scale3 r s) = Scale3 r $ filterEmpty s
filterEmpty (Rotate3 r s) = Rotate3 r $ filterEmpty s
filterEmpty (UnionR3 _ []) = Empty3
filterEmpty (UnionR3 r s) = UnionR3 r $ filter (not . isEmpty) $ fmap filterEmpty s
filterEmpty (DifferenceR3 r s ss) = DifferenceR3 r (filterEmpty s) $ fmap filterEmpty ss
filterEmpty (IntersectR3 r ss) = IntersectR3 r $ fmap filterEmpty ss
filterEmpty (Shell3 r s) = Shell3 r $ filterEmpty s
filterEmpty (Outset3 r s) = Outset3 r $ filterEmpty s
filterEmpty (Complement3 s) = Complement3 $ filterEmpty s
filterEmpty (Mirror3 r s) = Mirror3 r $ filterEmpty s
filterEmpty x@Full3{} = x
filterEmpty x@CubeR{} = x
filterEmpty x@Sphere{} = x
filterEmpty x@Cylinder{} = x
filterEmpty x = x

isEmpty :: SymbolicObj3 -> Bool
isEmpty Empty3 = True
isEmpty _ = False

