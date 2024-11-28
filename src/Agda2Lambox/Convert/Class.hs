module Agda2Lambox.Convert.Class where

import Agda2Lambox.Monad ( C )

type (:~>) a b = a -> C b

-- | Converting between two types @a@ and @b@ under Agda2Lambox's monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a :~> b
  convert = go
