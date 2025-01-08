module Agda2Lambox.Monad where

import Control.Monad.Reader ( ReaderT(runReaderT), local )
import Control.Monad.State ( StateT, runStateT )

import Agda ( TCM , QName)

-- | TCM monad extended with a custom environment and state.
type C = ReaderT Env TCM

runC0 :: C a -> TCM a
runC0 m = runReaderT m initEnv

-- | λ□ conversion environment.
data Env = Env
  { mutuals   :: [QName]
     -- ^ When we compile mutual definitions, they are introduced at the top of the local context.
  , boundVars :: Int
  }

initEnv :: Env
initEnv = Env
  { mutuals   = []
  , boundVars = 0
  }

inMutuals :: [QName] -> C a -> C a
inMutuals ds = local $ \e -> e
  { mutuals = reverse ds
  }

inBoundVars :: Int -> C a -> C a
inBoundVars n = local $ \e -> e
  { boundVars = boundVars e + n }

inBoundVar :: C a -> C a
inBoundVar = inBoundVars 1
