module Agda2Lambox.Monad where

import Control.Monad.Reader ( ReaderT(runReaderT) , local , ask  )
import Control.Monad.State ( StateT, runStateT )

import Agda ( TCM , QName)

-- | TCM monad extended with a custom environment and state.
type C = ReaderT Env TCM

runC0 :: QName -> C a -> TCM a
runC0 defName m = runReaderT m (initEnv defName)

data Env = Env
  { currentDef :: QName
  , boundVars  :: Int
  }

initEnv :: QName -> Env
initEnv defName = Env defName 0

inBoundVar :: C a -> C a
inBoundVar = local $ \ e -> e
  { boundVars = boundVars e + 1 }

inDefName :: QName -> C a -> (Int -> C a) -> C a
inDefName name m f = do
  defName <- currentDef <$> ask
  if name == defName then
    ask >>= \e -> f (boundVars e)
  else
    m
