{-# LANGUAGE PatternSynonyms #-}

module Merge where

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Data.Foldable
import Linear (Quaternion(Quaternion))
import Graphics.Implicit.Primitives (rotateQ)
import Data.Tuple (swap)

merge :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
merge = mergeR 0

mergeR :: Double -> SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
mergeR r x (UnionR3 d l_s) = foldl' (mergeR (r + d)) x l_s
mergeR _ x (DifferenceR3 d l l_s) = differenceR d (x <> l) l_s
mergeR _ x (IntersectR3 d (l : l_s)) = intersectR d $ union [x, l] : l_s
mergeR r x (Translate3 pddd s) = translate pddd $ mergeR r (translate (negate pddd) x) s
mergeR r x (Scale3 pddd s) = scale pddd $ mergeR r (scale (allthree (1 /) pddd) x) s
mergeR r x (Rotate3 pddd@(Quaternion q v3) s) = rotateQ pddd $ mergeR r (rotateQ (Quaternion (negate q) v3) x) s
mergeR r x y = unionR r [x, y]

infixl 6 +-
(+-) :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
(+-) = merge

merging :: SymbolicObj3 -> [SymbolicObj3] -> SymbolicObj3
merging = foldl' merge

inverse :: SymbolicObj3 -> SymbolicObj3
inverse (Inverse x) = x
inverse x = Inverse x

rip :: SymbolicObj3 -> (SymbolicObj3, SymbolicObj3)
rip (Inverse x)          = swap $ rip x
rip (UnionR3 _ l_s)      = foldMap rip l_s
rip (IntersectR3 _ _)    = error "tried to rip an intersection and i avent thought of it"
rip (Translate3 _ Empty) = (mempty, mempty)
rip (Translate3 xyz l)   = both (translate xyz) $ rip l
rip (Scale3 _ Empty)     = (mempty, mempty)
rip (Scale3 xyz l)       = both (scale xyz) $ rip l
rip (Rotate3 _ Empty)    = (mempty, mempty)
rip (Rotate3 xyz l)      = both (Rotate3 xyz) $ rip l
rip (DifferenceR3 r a b) =
  let (keep_a, cut_a) = rip a
   in (keep_a, unionR r $ cut_a : b)
rip x                    = (x, mempty)

carve :: SymbolicObj3 -> SymbolicObj3
carve = uncurry difference . fmap pure . rip

pattern Empty :: SymbolicObj3
pattern Empty = UnionR3 0 []

pattern Inverse :: SymbolicObj3 -> SymbolicObj3
pattern Inverse x = Scale3 (1, 1, 1) x

-- inverse :: SymbolicObj3 -> SymbolicObj3
-- inverse

-- rip x (DifferenceR3 d l l_s) = differenceR d (x <> l) l_s
-- rip x (IntersectR3 d (l : l_s)) = intersectR d $ union [x, l] : l_s
-- rip x (Translate3 pddd s) = translate pddd $ mergeR r (translate (negate pddd) x) s
-- rip x (Scale3 pddd s) = scale pddd $ mergeR r (scale (allthree (1 /) pddd) x) s
-- rip x (Rotate3 pddd s) = rotate3 pddd $ mergeR r (rotate3 (negate pddd) x) s
-- rip x y = unionR r [x, y]


