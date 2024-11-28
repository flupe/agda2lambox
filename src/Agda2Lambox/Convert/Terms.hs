module Agda2Lambox.Convert.Terms () where

import Utils

import qualified Agda as A
import Agda.Lib ( )
import Agda.Utils

import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
-- import Agda2Lambox.Convert.Names
-- import Agda2Lambox.Convert.Literals
-- import Agda2Lambox.Convert.Builtins
-- import Agda2Lambox.Convert.Types

-- | Compiling (treeless) Agda terms into Lambox expressions.
instance A.TTerm ~> L.Term where
  go t = do
   report $ "* compiling tterm: " <> pp t
   return L.Box
