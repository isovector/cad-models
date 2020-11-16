module Roomba where

import Lib
import StdParts
import Graphics.Implicit.Primitives (Object(getBox))
import Data.Maybe (fromJust)


main :: IO ()
main = do
  writeSTL 1 "/tmp/roomba.stl" $ do
    fromJust $ pack3 (500, 500) 10 $ split basePlate
    -- union
    --   [ head $ split basePlate
    --   ]



------------------------------------------------------------------------------

vacuumBagRadius :: R
vacuumBagRadius = 55


additionalRadius :: R
additionalRadius = 130 - vacuumBagRadius

baseThickness :: R
baseThickness = 2.5

wheelRadPos :: R
wheelRadPos = vacuumBagRadius + additionalRadius + yellowWheelRadius / 2

wheelDegPos :: R
wheelDegPos = 140

halfPos :: R
halfPos = vacuumBagRadius + additionalRadius /2


agitatorDegPos :: R
agitatorDegPos = 55

------------------------------------------------------------------------------

basePlate :: SymbolicObj3
basePlate = intersect
  [ union
    [ translate (0, 0, -baseThickness) $
        difference
          [ cylinder (vacuumBagRadius + additionalRadius) baseThickness
          , cylinder  vacuumBagRadius baseThickness
          , symmetrically halfPos agitatorDegPos agitatorHoleBB
          , symmetrically wheelRadPos wheelDegPos yellowWheelBB
          ]
    , translate (mk3 (-3) 58 0) $
        withPolarPos wheelRadPos (-wheelDegPos) $
          rotate3 (degZ (-90)) $
            wedge 15 15 15
      -- centeredBox (baseThickness * 2) 20 30
      -- wheel motor
    , symmetrically (halfPos + 45 / 2) 145 $ extrudedSlot 2 2 $ centeredBox 19 65 10
    , symmetrically halfPos agitatorDegPos agitatorSlot
    , withPolarPos halfPos (-80) $ rotate90Z arduinoMiniSlot
    , withPolarPos halfPos (-105) $ rotate90Z miniBreadboardSlot
    , withPolarPos halfPos 180 l298nSlot
    , withPolarPos halfPos 98 $ doubleAAHolderSlot
    , withPolarPos halfPos 0 $ rotate90Z doubleAAHolderSlot
    ]
  , basePlateBounding
  ]

rotate90Z :: SymbolicObj3 -> SymbolicObj3
rotate90Z = rotate3 (degZ 90)

basePlateBounding :: SymbolicObj3
basePlateBounding = translate (0, 0, -baseThickness) $ cylinder (vacuumBagRadius + additionalRadius) 100

withPolarPos
    :: R             -- ^ r
    -> R             -- ^ theta in degrees
    -> SymbolicObj3  -- ^ obj to position
    -> SymbolicObj3
withPolarPos r theta = translate (basePolarPos r $ deg theta)

symmetrically :: R -> R -> SymbolicObj3 -> SymbolicObj3
symmetrically r theta obj = union
  [ withPolarPos r theta obj
  , withPolarPos r (-theta) obj
  ]



basePolarPos :: R -> R -> R3
basePolarPos r theta = expandR2 0 $ unpackV2 $ rotMat theta !* V2 0 r

rotMat :: R -> M22 Double
rotMat theta =
  V2 (V2 ct (-st))
     (V2 st ct)
  where
    ct = cos theta
    st = sin theta


agitatorHoleBB :: SymbolicObj3
agitatorHoleBB = centeredBox 15 15 25

agitatorSlot :: SymbolicObj3
agitatorSlot = extrudedSlot 2 2 $ centeredBox 21 21 25


split :: SymbolicObj3 -> [SymbolicObj3]
split obj =
  let ((x1, y1, z1), (x2, y2, z2)) = getBox obj
      w = x2 - x1
      d = y2 - y1
      h = z2 - z1
      b = centeredBox w d h
   in [ intersect [ obj, translate (mk3 x1 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y1 z1) b ]
      , intersect [ obj, translate (mk3 x2 y2 z1) b ]
      , intersect [ obj, translate (mk3 x1 y2 z1) b ]
      ]


