module Agda2Lambox.Monad where

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( StateT, runStateT )

import Agda ( TCM )

-- | TCM monad extended with a custom environment and state.
type C = TCM

runC0 :: C a -> TCM a
runC0 = id
