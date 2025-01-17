{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , CompileState(..)
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

-- | Backend compilation state.
data CompileState = CompileState
  { compiledDefs :: Set QName
    -- ^ Names already compiled.
  , requiredDefs :: Set QName
    -- ^ Names in the compilation queue.
  , compileQueue :: EphemeralQueue QName
    -- ^ Compilation queue.
  }

-- NOTE(flupe):
-- - We use a Queue to do a BFS traversal of definitions rather than DFS.
--   i.e try to compile "related" definitions together.
-- - We use an EphemeralQueue because we don't rely on persistence at all.

-- | Initial compile state. Empty queue, nothing has been compiled.
initState :: CompileState
initState = CompileState
  { compiledDefs = Set.empty
  , requiredDefs = Set.empty
  , compileQueue = Queue.empty
  }

-- | Backend compilation monad.
newtype CompileM a = Compile (StateT CompileState TCM a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState CompileState)
  deriving newtype (MonadIO, MonadFail)
  deriving newtype (MonadTCEnv, MonadTCState, HasOptions, MonadTCM)

-- | Run a compilation unit in TCM.
compile :: CompileM a -> TCM a
compile (Compile c) = evalStateT c initState

-- | Require a definition to be compiled.
requireDef :: QName -> CompileM ()
requireDef q = do
  required <- gets requiredDefs
  compiled <- gets compiledDefs
  -- a name is only added to the queue if it isn't already there and hasn't been compiled yet
  unless (Set.member q required || Set.member q compiled) $
    modify \ s -> s
      { requiredDefs = Set.insert q required
      , compileQueue = Queue.enqueue q $ compileQueue s
      }

-- | Get the next qname to be compiled, if there is one.
--   The name is immediately marked as being compiled.
nextUnit :: CompileM (Maybe QName)
nextUnit = do
  CompileState{..} <- get
  case compileQueue of
    Empty        -> pure Nothing
    Full q queue -> Just q <$ put CompileState
      { compiledDefs = Set.insert q compiledDefs
      , requiredDefs = Set.delete q requiredDefs
      , compileQueue = queue
      }

-- | Run the processing function as long as there are names to be compiled.
compileLoop :: (QName -> CompileM ()) -> CompileM ()
compileLoop step =
  nextUnit >>= \case
    Nothing -> pure ()
    Just q  -> step q *> compileLoop step
