{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Text where

import Lib
import Diagrams.Path
import Diagrams.Segment (Segment(..), Closed)
import Diagrams (viewLoc, Point (..), Located, Offset(OffsetClosed))
import Diagrams.Trail
import Graphics.SVGFonts (textSVG)
import Control.Monad
import Data.List (nub, genericLength, genericIndex)
import Graphics.Implicit.MathUtil (distFromLineSeg)

main :: IO ()
main = do
  writeSTL 0.25 "/tmp/roomba.stl" $ do
    extrude 1 $ text 5 "hello"

text :: R -> String -> SymbolicObj2
text h str =
  let points = fmap unpackV2 $ renderPath $ textSVG str h
      poly = polygonR 0 points
   in poly


renderPath :: (Ord n, Floating n) => Path V2 n -> [V2 n]
renderPath = foldMap renderTrail . op Path

renderTrail :: forall n. (Ord n, Floating n) => Located (Trail V2 n) -> [V2 n]
renderTrail (viewLoc -> (P xy, t)) =
  fixOffsets (withTrail renderLine renderLoop t)
  where
    fixOffsets :: [V2 n] -> [V2 n]
    fixOffsets = scanl (+) xy

    renderLine :: Trail' Line V2 n -> [V2 n]
    renderLine = fixOffsets . foldMap renderSeg . lineSegments

    renderLoop :: Trail' Loop V2 n -> [V2 n]
    renderLoop lp =
      case loopSegments lp of
        -- let z handle the last segment if it is linear
        (segs, Linear _) -> foldMap renderSeg segs

        -- otherwise we have to emit it explicitly
        _ -> foldMap renderSeg (lineSegments . cutLoop $ lp)

renderSeg :: Floating n => Segment Closed V2 n -> [V2 n]
renderSeg (Linear (OffsetClosed v2)) = [v2]
renderSeg (Cubic p1 p2 (OffsetClosed p3)) = cubicOffsets p1 p2 p3


cubicOffsets :: Floating n => V2 n -> V2 n -> V2 n -> [V2 n]
cubicOffsets p1 p2 p3 =
  let sample t = sampleCubic t (V2 0 0) p1 p2 p3
      p1' = sample 0.2
      p2' = sample 0.4
      p3' = sample 0.6
      p4' = sample 0.8
   in [ p1'
      , p2' - p1'
      , p3' - p2'
      , p4' - p3'
      , p3  - p4'
      ]

sampleCubic :: Floating n => n -> V2 n -> V2 n -> V2 n -> V2 n -> V2 n
sampleCubic t p0 p1 p2 p3 =
  let minust = 1 - t
   in minust^3 *^ p0
    + (3 * minust^2 * t) *^ p1
    + (3 * minust * t^2) *^ p2
    + t^3 *^ p3


renderSeg2 :: Located (Segment Closed V2 Double) -> [V2 Double]
renderSeg2 l =
  case viewLoc l of
    (p, Linear (OffsetClosed v)) -> [p', (p' + v)]
      where
        p' = op P p
    (p, Cubic u1 u2 (OffsetClosed u3)) ->
        [ sampleCubic 0 q0 q1 q2 q3
        , sampleCubic 0.2 q0 q1 q2 q3
        , sampleCubic 0.4 q0 q1 q2 q3
        , sampleCubic 0.6 q0 q1 q2 q3
        , sampleCubic 0.8 q0 q1 q2 q3
        , sampleCubic 1 q0 q1 q2 q3
        ]
      where
        (q0, q1, q2, q3) = (op P p, q0 + u1, q0 + u2, q0 + u3)

renderPath2 :: Path V2 Double -> [[V2 Double]]
renderPath2 = fmap (join . fmap renderSeg2) . pathLocSegments

getImplicit2 :: [R2] -> R2 -> R
getImplicit2 points =
    \p -> let
        pair :: Int -> (ℝ2,ℝ2)
        pair n = (points `genericIndex` n, points `genericIndex` mod (n + 1) (genericLength points) )
        pairs :: [(ℝ2,ℝ2)]
        pairs =  [ pair n | n <- [0 .. genericLength points - 1] ]
        relativePairs =  fmap (\(a,b) -> (a ^-^ p, b ^-^ p) ) pairs
        crossing_points =
            [ x2 - y2*(x2-x1)/(y2-y1)
            | ((x1,y1), (x2,y2)) <-relativePairs
            , ( (y2 <= 0) && (y1 >= 0) ) || ( (y2 >= 0) && (y1 <= 0) ) ]
        -- FIXME: use partition instead?
        seemsInRight = odd . length . filter (>0) $ nub crossing_points
        seemsInLeft = odd . length . filter (<0) $ nub crossing_points
        isIn = seemsInRight && seemsInLeft
        dists :: [ℝ]
        dists = fmap (distFromLineSeg p) pairs
    in
        minimum dists * if isIn then -1 else 1

(^-^) :: R2 -> R2 -> R2
(^-^) (a, b) (a', b') = (a - a', b - b')

infixl 6 ^-^

