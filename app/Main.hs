module Main where

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Lib

------------------------------------------------------------------------------
-- |  ASSUME THE CAMERA IS FROM THE FRONT


------------------------------------------------------------------------------
-- | z is UP


boardWidth :: R
boardWidth = railWidth * 2 + wheelXClearance * 2 + wheelWidth

railWidth :: R
railWidth = 1.25

wheelWidth :: R
wheelWidth = 6

wheelRadius :: R
wheelRadius = 5.5

axelRadius :: R
axelRadius = 1

boardLength :: R
boardLength = 5

wheelZClearance :: R
wheelZClearance = 0.5

wheelXClearance :: R
wheelXClearance = 0.5

noseLength :: R
noseLength = wheelRadius * 2

tailLength :: R
tailLength = wheelRadius * 2

boardDepth :: R
boardDepth = 1.25


noseSym :: SymbolicObj3
noseSym = union
  [ centeredBox (mk3 0 (wheelRadius + wheelZClearance + half noseLength) 0)
      $ mk3 boardWidth noseLength boardDepth
  ]

tailSym :: SymbolicObj3
tailSym = union
  [ centeredBox (mk3 0 (negate $ wheelRadius + wheelZClearance + half tailLength) 0)
      $ mk3 boardWidth tailLength boardDepth
  ]


main :: IO ()
main = do
  writeSTL 0.5 "/tmp/res.stl" $ do
    union
      [ railLeft
      , railRight
      , wheel
      , axel
      , noseSym
      , tailSym
      ]

half :: R -> R
half = (/ 2)

railLeft :: SymbolicObj3
railLeft = translate (allthree negate railPos) railSym

railRight :: SymbolicObj3
railRight = translate railPos railSym

railPos :: R3
railPos = mk3 (half wheelWidth + half railWidth + wheelXClearance) 0 0


railSym :: SymbolicObj3
railSym = centeredBox (mk3 0 0 0)
        $ mk3 railWidth (wheelRadius * 2 + wheelZClearance * 2) boardDepth


wheel :: SymbolicObj3
wheel = translate (mk3 (negate $ wheelWidth / 2) 0 0)
      $ rotate3 (degY 90)
      $ extrudeR 2 (circle wheelRadius) wheelWidth


axel :: SymbolicObj3
axel = translate (mk3 (negate $ wheelWidth / 2 + wheelXClearance) 0 0)
     $ rotate3 (degY 90)
     $ extrude (wheelWidth + 2 * wheelXClearance)
     $ circle axelRadius

-- noseSym :: SymbolicObj3
-- noseSym = box

centeredBox :: R3 -> R3 -> SymbolicObj3
centeredBox (x, y, z) (w, d, h) =
  rect3R 0 (mk3 (x - half w) (y - half d) (z - half h))
           (mk3 (x + half w) (y + half d) (z + half h))

