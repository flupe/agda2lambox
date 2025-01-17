{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , requireDef
  , compile
  , compileLoop
  ) where

import Control.Monad ( unless )
import Control.Monad.State
import Control.Monad.IO.Class ( MonadIO )
import Data.Set ( Set )
import Data.Set qualified as Set
import Queue.Ephemeral ( EphemeralQueue(..) )
import Queue.Ephemeral qualified as Queue

import Agda.Syntax.Internal ( QName )
import Agda.TypeChecking.Monad.Base ( TCM, MonadTCEnv, MonadTCM, MonadTCState, MonadTCEnv, HasOptions )
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

-- | Initial compile state. Empty queue, no name is known.
initState :: CompileState
initState = CompileState
  { seenDefs     = Set.empty
  , compileQueue = Queue.empty
  }

-- | Backend compilation monad.
newtype CompileM a = Compile (StateT CompileState TCM a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadIO, MonadFail)
  deriving newtype (MonadTCEnv, MonadTCState, HasOptions, MonadTCM)

-- deriving newtype (MonadState CompileState)
-- NOTE(flupe): safe to not export this, to make sure the queue is indeed ephemeral

-- | Run a compilation unit in TCM.
compile :: CompileM a -> TCM a
compile (Compile c) = evalStateT c initState

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
compileLoop :: (QName -> CompileM (Maybe a)) -> CompileM [a]
compileLoop step =
  nextUnit >>= \case
    Nothing -> pure []
    Just q  -> mcons <$> step q <*> compileLoop step
