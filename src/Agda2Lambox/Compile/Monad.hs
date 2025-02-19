{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , requireDef
  , compileLoop
  ) where

import Control.Monad ( unless )
import Control.Monad.State
import Control.Monad.IO.Class ( MonadIO )
import Data.Set ( Set )
import Data.Set qualified as Set
import Queue.Ephemeral ( EphemeralQueue(..) )
import Queue.Ephemeral qualified as Queue

import Agda.Compiler.Backend ( QName, Definition, getConstInfo )
import Agda.TypeChecking.Monad.Base ( TCM, MonadTCEnv, MonadTCM(liftTCM), MonadTCState, MonadTCEnv, HasOptions )
import Agda.Utils.List ( mcons )

-- | Backend compilation state.
data CompileState = CompileState
  { seenDefs     :: Set QName
    -- ^ Names that we have seen, either already compiled or in the queue.
  , compileQueue :: EphemeralQueue QName
    -- ^ Compilation queue.
  }

-- NOTE(flupe):
-- - We use a Queue to do a BFS traversal of definitions rather than DFS.
--   i.e try to compile "related" definitions together.
-- - We use an EphemeralQueue because we don't rely on persistence at all.

-- | Initial compile state, with a set of names required for compilation.
initState :: [QName] -> CompileState
initState qs = CompileState
  { seenDefs     = Set.fromList qs
  , compileQueue = Queue.fromList qs
  }

-- | Backend compilation monad.
newtype CompileM a = Compile (StateT CompileState TCM a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadIO, MonadFail)
  deriving newtype (MonadTCEnv, MonadTCState, HasOptions, MonadTCM)

-- deriving newtype (MonadState CompileState)
-- NOTE(flupe): safe to not export this, to make sure the queue is indeed ephemeral

-- | Require a definition to be compiled.
requireDef :: QName -> CompileM ()
requireDef q = Compile $ do
  seen <- gets seenDefs
  -- a name is only added to the queue if we haven't seen it yet
  unless (Set.member q seen) $
    modify \ s -> s
      { seenDefs     = Set.insert q seen
      , compileQueue = Queue.enqueue q $ compileQueue s
      }

-- | Get the next qname to be compiled, if there is one.
nextUnit :: CompileM (Maybe QName)
nextUnit = Compile $
  gets compileQueue >>= \case
    Empty        -> pure Nothing
    Full q queue -> Just q <$ modify \s -> s { compileQueue = queue }

-- | Run the processing function as long as there are names to be compiled.
compileLoop
  :: (Definition -> CompileM (Maybe a)) -- ^ The compilation function
  -> [QName]                            -- ^ Names to compile
  -> TCM [a]
compileLoop step = evalStateT unloop . initState
  where
  loop@(Compile unloop) = nextUnit >>= \case
    Nothing -> pure []
    Just q  -> do
      mr <- step =<< (liftTCM $ getConstInfo q)
      mcons mr <$> loop
