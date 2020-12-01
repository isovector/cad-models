{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Alignment where

import           Control.Lens hiding (plate, both)
import           Graphics.Implicit.Primitives
import qualified Linear as L
import           Types


getOrigin :: Object obj vec => obj -> vec
getOrigin = fst . getBox

getExtent :: Object obj vec => obj -> vec
getExtent = snd . getBox

class StupidImplicitVector a where
  zero :: a

instance StupidImplicitVector R2 where
  zero = mk2 0 0

instance StupidImplicitVector R3 where
  zero = mk3 0 0 0



slamming
    :: (Num a, Object obj vec, StupidImplicitVector vec)
    => Lens' vec a
    -> (obj -> vec)
    -> obj
    -> obj
slamming l boxsel obj =
  let dist = negate $ boxsel obj ^. l
      dpos = zero & l .~ dist
   in translate dpos obj

slamBottom :: SymbolicObj3 -> SymbolicObj3
slamBottom = slam OnBottom

slamTop :: SymbolicObj3 -> SymbolicObj3
slamTop = slam OnTop

slamLeft :: (Object obj vec, StupidImplicitVector vec, Field1 vec vec Double Double) => obj -> obj
slamLeft = slamming _1 getOrigin

slamRight :: (Object obj vec, StupidImplicitVector vec, Field1 vec vec Double Double) => obj -> obj
slamRight = slamming _1 getExtent

slamFront :: (Object obj vec, StupidImplicitVector vec, Field2 vec vec Double Double) => obj -> obj
slamFront = slamming _2 getOrigin

slamBack :: (Object obj vec, StupidImplicitVector vec, Field2 vec vec Double Double) => obj -> obj
slamBack = slamming _2 getExtent

data Alignment = OnLeft | OnRight | OnTop | OnBottom | OnFront | OnBack
  deriving (Eq, Ord, Show, Enum, Bounded)


alignmentAxis :: Alignment -> Lens' R3 R
alignmentAxis = \case
   OnLeft   -> _1
   OnRight  -> _1
   OnFront  -> _2
   OnBack   -> _2
   OnTop    -> _3
   OnBottom -> _3

alignmentComplement :: Alignment -> Alignment
alignmentComplement = \case
   OnLeft   -> OnRight
   OnRight  -> OnLeft
   OnFront  -> OnBack
   OnBack   -> OnFront
   OnTop    -> OnBottom
   OnBottom -> OnTop

alignmentBB :: Alignment -> SymbolicObj3 -> R3
alignmentBB = \case
   OnLeft   -> getOrigin
   OnRight  -> getExtent
   OnFront  -> getOrigin
   OnBack   -> getExtent
   OnTop    -> getExtent
   OnBottom -> getOrigin

slam :: Alignment -> SymbolicObj3 -> SymbolicObj3
slam a = slamming (alignmentAxis a) (alignmentBB a)

flush :: Alignment -> [SymbolicObj3] -> SymbolicObj3
flush = foldMap . slam

abut :: Alignment -> SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
abut a s1 s2 = slam a s1 <> slam (alignmentComplement a) s2

center3 :: SymbolicObj3 -> SymbolicObj3
center3 obj =
  let (packV3 -> orig, packV3 -> ext) = getBox obj
      dpos = negate $ orig + (ext - orig)  L.^* 0.5
   in translate (unpackV3 dpos) obj

