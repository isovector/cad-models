module Roomba where

import Lib
import StdParts
import Graphics.SVGFonts (textSVG)


main :: IO ()
main = do
  writeSTL 1 "/tmp/roomba.stl" $ do
    union
      [ basePlate
      ]



------------------------------------------------------------------------------

vacuumBagRadius :: R
vacuumBagRadius = 55


additionalRadius :: R
additionalRadius = 130 - vacuumBagRadius

baseThickness :: R
baseThickness = 2.5

wheelRadPos :: R
wheelRadPos = vacuumBagRadius + additionalRadius + yellowWheelRadius / 2

halfPos :: R
halfPos = vacuumBagRadius + additionalRadius /2


------------------------------------------------------------------------------

basePlate :: SymbolicObj3
basePlate = union
  [ translate (0, 0, -baseThickness) $ difference
    [ cylinder (vacuumBagRadius + additionalRadius) baseThickness
    , cylinder  vacuumBagRadius baseThickness
    , symmetrically halfPos 60 agitatorHoleBB
    , symmetrically wheelRadPos 140 yellowWheelBB
    ]
    -- wheel motor
  , symmetrically (halfPos + 45 / 2) 145 $ extrudedSlot 2 2 $ centeredBox 19 65 10
  , symmetrically halfPos 60 agitatorSlot
  , withPolarPos halfPos 90 arduinoMiniSlot
  , withPolarPos (halfPos + 5) 180 l298nSlot
  , withPolarPos halfPos (-90) miniBreadboardSlot
  ]

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



