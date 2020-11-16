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

main :: IO ()
main = do
  writeSTL 0.25 "/tmp/roomba.stl" $ do
    extrude 1 $ text 5 "hello"

text :: R -> String -> SymbolicObj2
text h str = polygonR 0 . fmap unpackV2 $ renderPath $ textSVG str h


renderPath :: (Ord n, Floating n) => Path V2 n -> [V2 n]
renderPath trs = foldMap renderTrail (op Path trs)

renderTrail :: forall n. (Ord n, Floating n) => Located (Trail V2 n) -> [V2 n]
renderTrail (viewLoc -> (P xy, t)) =
  xy : fixOffsets (withTrail renderLine renderLoop t)
  where
    fixOffsets :: [V2 n] -> [V2 n]
    fixOffsets = fmap (+xy) . scanl1 (+)

    renderLine :: Trail' Line V2 n -> [V2 n]
    renderLine = foldMap renderSeg . lineSegments

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
  let sample t = sampleCubic t p1 p2 p3
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

sampleCubic :: Floating n => n -> V2 n -> V2 n -> V2 n -> V2 n
sampleCubic t p1 p2 p3 =
  let minust = 1 - t
   in (3 * minust^2 * t) *^ p1
    + (3 * minust * t^2) *^ p2
    + t^3 *^ p3

