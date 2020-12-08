{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Alignment where

import Data.Monoid (Endo (..))
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

alignmentDirection :: Alignment -> Int
alignmentDirection = \case
   OnLeft   -> (-1)
   OnRight  -> 1
   OnFront  -> (-1)
   OnBack   -> 1
   OnTop    -> 1
   OnBottom -> (-1)

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

flush :: SymbolicObj3 -> R -> Alignment -> SymbolicObj3 -> SymbolicObj3
flush s1 r a s2 = inset s1 [Flush r a] s2

allFlush :: Alignment -> [SymbolicObj3] -> SymbolicObj3
allFlush a = center3 . foldMap (slam a)

abut :: SymbolicObj3 -> R -> Alignment -> SymbolicObj3 -> SymbolicObj3
abut s1 r a = inset s1 [Abut r a]

data Inset
  = Flush { insetDistance :: R, insetAlign :: Alignment }
  | Abut  { insetDistance :: R, insetAlign :: Alignment }
  deriving (Eq, Ord, Show)


positioning :: [(R, Alignment)] -> SymbolicObj3 -> SymbolicObj3
positioning as s = flip appEndo s $ flip foldMap as $ \(v, a) ->
  Endo $ \obj ->
    translate
       ( zero
         & alignmentAxis a .~ v * fromIntegral (alignmentDirection a))
       $ slam a obj

unwrapInset :: Inset -> (R, Alignment)
unwrapInset (Flush r a) = (-r, a)
unwrapInset (Abut r a)  = (r, a)


complementedIfShould :: Inset -> Alignment
complementedIfShould i@Flush{} = insetAlign i
complementedIfShould i@Abut{} = alignmentComplement $ insetAlign i

inset :: SymbolicObj3 -> [Inset] -> SymbolicObj3 -> SymbolicObj3
inset s1 as s2 = center3 $
  foldr (\i -> slam $ complementedIfShould i) s1 as
    <> positioning (fmap unwrapInset as) s2

center3 :: SymbolicObj3 -> SymbolicObj3
center3 obj =
  let (orig, ext) = getBox obj
      dpos = negate $ orig + (ext - orig)  L.^* 0.5
   in translate dpos obj

