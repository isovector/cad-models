{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Simplify where

import Control.Monad
import Generics.Deriving.Uniplate
import Graphics.Implicit.Definitions
import Graphics.Implicit
import Control.Lens hiding (rewrite, transform, Empty)
import GHC.Generics (Generic)

deriving anyclass instance Uniplate SymbolicObj3
deriving anyclass instance Uniplate SymbolicObj2
deriving anyclass instance Uniplate (SharedObj obj vec)

class Object obj vec => HasShared obj vec | obj -> vec where
  _Shared :: Prism' obj (SharedObj obj vec)

instance HasShared SymbolicObj2 ℝ2 where
  _Shared = prism' Shared2 $ \case
      Shared2 x -> Just x
      _         -> Nothing

instance HasShared SymbolicObj3 ℝ3 where
  _Shared = prism' Shared3 $ \case
      Shared3 x -> Just x
      _         -> Nothing


pattern Shared :: HasShared obj vec => SharedObj obj vec -> obj
pattern Shared v <- (preview _Shared -> Just v)
  where
    Shared v = _Shared # v


simplifyShared
    :: (HasShared obj vec, Num vec)
    => SharedObj obj vec
    -> Maybe (Either obj (SharedObj obj vec))
simplifyShared = \case
    (Translate v1 (Shared (Translate v2 s)))
      -> Just (Right $ Translate (v1 + v2) s)
    (Translate _ (Shared Empty))
      -> Just $ Right Empty
    (Translate _ (Shared Full))
      -> Just $ Right Full
    (Scale v1 (Shared (Scale v2 s)))
      -> Just $ Right $ Scale (v1 + v2) s
    (Scale _ (Shared Empty))
      -> Just $ Right Empty
    (Scale _ (Shared Full))
      -> Just $ Right Full
    (UnionR _ [a])
      -> Just $ Left a
    (DifferenceR _ (Shared Empty) _)
      -> Just $ Right Empty
    (DifferenceR _ a [])
      -> Just $ Left a
    (IntersectR _ [a])
      -> Just $ Left a
    _ -> Nothing


simplify :: (Num vec, Object obj vec, HasShared obj vec, Uniplate obj) => obj -> obj
simplify = rewrite $
  fmap (either id (review _Shared)) . simplifyShared
    <=< preview _Shared

yo :: SymbolicObj3
yo = mconcat $ pure $ translate 0 $ translate 1 $ translate 2 $ emptySpace


data Foo = In | Out | Loop Bar deriving (Generic, Show)
data Bar = BackToFoo Foo deriving (Generic, Show)

deriving anyclass instance Uniplate Foo
deriving anyclass instance Uniplate Bar

nice :: Foo -> Foo
nice = rewrite $ \case
    In -> Just Out
    _ -> Nothing

doesnt_rewrite = nice $ Loop $ BackToFoo In

