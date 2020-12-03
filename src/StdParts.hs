module StdParts where

import Lib


yellowWheelBB :: SymbolicObj3
yellowWheelBB =
  centeredBox
    wheel_width
    wheel_diameter
    wheel_diameter
  where
    wheel_width = 28
    wheel_diameter = yellowWheelRadius * 2


yellowWheelRadius :: R
yellowWheelRadius = 70 / 2


yellowWheelMotorWidth :: R
yellowWheelMotorWidth = 19

yellowWheelMotorAxelOffset :: R
yellowWheelMotorAxelOffset = 11.3

yellowWheelMotorSlot :: SymbolicObj3
yellowWheelMotorSlot = extrudedSlot 2 2 yellowWheelMotorBB

yellowWheelMotorBB :: SymbolicObj3
yellowWheelMotorBB = centeredBox 19 48.3 22.5


tabXPosFacing :: SymbolicObj3
tabXPosFacing = box 2 5 7


tabXNegFacing :: SymbolicObj3
tabXNegFacing = rotate3 (degZ pi) $ box 2 5 7


arduinoMiniSlot :: SymbolicObj3
arduinoMiniSlot = extrudedSlot 2 2 $ centeredBox 45 18.5 10


l298nSlot :: SymbolicObj3
l298nSlot = extrudedSlot 2 2 $ centeredBox 43.5 43.5 10


miniBreadboardSlot :: SymbolicObj3
miniBreadboardSlot = extrudedSlot 2 2 $ centeredBox 35 47 10


doubleAAHolderSlot :: SymbolicObj3
doubleAAHolderSlot = extrudedSlot 2 2 $ centeredBox 32.4 85.5 10


ovonicLipoBatterySlot :: SymbolicObj3
ovonicLipoBatterySlot = extrudedSlot 2 2 $ centeredBox 35 76.5 10

