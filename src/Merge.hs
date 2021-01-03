{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall        #-}

module Merge where

import GHC.Generics
import Graphics.Implicit
import Graphics.Implicit.Definitions
import Data.Foldable
import Linear (Quaternion(Quaternion))
import Graphics.Implicit.Primitives (pattern Shared, outset, rotateQ)
import Data.Tuple (swap)

merge :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
merge = mergeR 0

mergeR :: Double -> SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
mergeR r x (Shared3 (UnionR d l_s)) = foldl' (mergeR (r + d)) x l_s
mergeR _ x (Shared3 (DifferenceR d l l_s)) = differenceR d (x <> l) l_s
mergeR _ x (Shared3 (IntersectR d (l : l_s))) = intersectR d $ union [x, l] : l_s
mergeR r x (Shared3 (Translate pddd s)) = translate pddd $ mergeR r (translate (negate pddd) x) s
mergeR r x (Shared3 (Scale pddd s)) = scale pddd $ mergeR r (scale (fmap (1 /) pddd) x) s
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
rip (Shared (UnionR _ l_s))      = foldMap rip l_s
rip x@(Shared (IntersectR{}))      = (x, mempty)
rip (Shared (Translate xyz l))   = both (translate xyz) $ rip l
rip (Shared (Scale xyz l))       = both (scale xyz) $ rip l
rip (Rotate3 xyz l)      = both (rotateQ xyz) $ rip l
rip (Shared (Mirror xyz l))      = both (mirror xyz) $ rip l
rip (Shared (DifferenceR r a b)) =
  let (keep_a, cut_a) = rip a
   in (keep_a, unionR r $ cut_a : b)
rip x                    = (x, mempty)

cut :: SymbolicObj3 -> SymbolicObj3
cut = Shared3 . DifferenceR 0 mempty . pure . snd . rip

carve :: SymbolicObj3 -> SymbolicObj3
carve = uncurry difference . fmap pure . rip

pattern Inverse :: SymbolicObj3 -> SymbolicObj3
pattern Inverse x = Shared3 (Scale (V3 1 1 1) x)

pattern Empty3 :: SymbolicObj3
pattern Empty3 = Shared3 Empty

isEmpty :: SymbolicObj3 -> Bool
isEmpty (Shared3 Empty) = True
isEmpty _ = False

simplify :: SymbolicObj3 -> SymbolicObj3
simplify = id

