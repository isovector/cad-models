module Merge where

import Lib
import Graphics.Implicit.Definitions
import Data.Foldable

merge :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
merge = mergeR 0

mergeR :: R -> SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
mergeR r x (UnionR3 d l_s) = foldl' (mergeR (r + d)) x l_s
mergeR _ x (DifferenceR3 d (l : l_s)) = differenceR d $ union [x, l] : l_s
mergeR _ x (IntersectR3 d (l : l_s)) = intersectR d $ union [x, l] : l_s
mergeR r x (Translate3 pddd s) = translate pddd $ mergeR r (translate (negate pddd) x) s
mergeR r x (Scale3 pddd s) = scale pddd $ mergeR r (scale (allthree (1 /) pddd) x) s
mergeR r x (Rotate3 pddd s) = rotate3 pddd $ mergeR r (rotate3 (negate pddd) x) s
mergeR r x y = unionR r [x, y]

infixl 6 +-
(+-) :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
(+-) = merge

merging :: SymbolicObj3 -> [SymbolicObj3] -> SymbolicObj3
merging = foldl' merge



