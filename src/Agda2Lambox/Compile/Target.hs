{-# LANGUAGE DataKinds, GADTs #-}
-- | Compile targets for the backend
module Agda2Lambox.Compile.Target 
  ( Typing(..)
  , Target(..)
  , WhenTyped(..)
  , extract
  , catchall
  ) where

import Control.DeepSeq ( NFData(rnf) )
import Data.Kind ( Type )

-- | Supported targets.
data Typing = Typed | Untyped

-- | Compile targets, indexed by the kind of target.
data Target :: Typing -> Type where
  ToTyped   :: Target Typed
  ToUntyped :: Target Untyped

-- | Type wrapper that contains a value iff we're in the typed setting.
data WhenTyped a :: Typing -> Type where
  None ::      WhenTyped a Untyped
  Some :: a -> WhenTyped a Typed

-- | Retrieve a value when it's there for sure.
extract :: WhenTyped a Typed ->  a
extract (Some x) = x

-- | Wrap a default value.
catchall :: Target t -> a -> WhenTyped a t
catchall ToUntyped x = None
catchall ToTyped   x = Some x

instance NFData (Target t) where
  rnf ToTyped   = ()
  rnf ToUntyped = ()
