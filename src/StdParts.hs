module StdParts where

import Lib
import Graphics.Implicit


yellowWheelBB :: SymbolicObj3
yellowWheelBB =
  centeredBox
    (wheel_width + total_clearance)
    (wheel_diameter + total_clearance)
    (wheel_diameter + total_clearance)
  where
    wheel_width = 26
    wheel_diameter = yellowWheelRadius * 2
    clearance = 4
    total_clearance = clearance * 2

yellowWheelRadius :: R
yellowWheelRadius = 68 / 2


yellowWheelMotorWidth :: R
yellowWheelMotorWidth = 19

tabXPosFacing :: SymbolicObj3
tabXPosFacing = box 2 5 7

tabXNegFacing :: SymbolicObj3
tabXNegFacing = rotate3 (degZ pi) $ box 2 5 7

arduinoMiniSlot :: SymbolicObj3
arduinoMiniSlot = extrudedSlot 2 2 $ centeredBox 18 45 10

l298nSlot :: SymbolicObj3
l298nSlot = extrudedSlot 2 2 $ centeredBox 88 44 10

miniBreadboardSlot :: SymbolicObj3
miniBreadboardSlot = extrudedSlot 2 2 $ centeredBox 35 47 10

doubleAAHolderSlot :: SymbolicObj3
doubleAAHolderSlot = extrudedSlot 2 2 $ centeredBox 32.4 85.5 10

