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

agitatorHoleBB :: SymbolicObj3
agitatorHoleBB = centeredBox 15 15 25

agitatorSlot :: SymbolicObj3
agitatorSlot = extrudedSlot 2 2 $ centeredBox 21 21 25



