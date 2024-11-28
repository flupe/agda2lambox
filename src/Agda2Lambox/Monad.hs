module Agda2Lambox.Monad where

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( StateT, runStateT )

import Agda ( TCM )

-- | TCM monad extended with a custom environment and state.
type C = StateT State (ReaderT Env TCM)

runC :: State -> C a -> TCM (a, State)
runC s0 k = runReaderT (runStateT k s0) initEnv

runC0 :: C a -> TCM (a, State)
runC0 = runC initState

-- | Environment tracking which part of a definition we are currently compiling.
data Env = Env {}

initEnv :: Env
initEnv = Env {}

-- | Compilation state.
data State = State {}

initState :: State
initState = State {}
