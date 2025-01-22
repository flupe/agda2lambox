{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , requireDef
  , compileLoop
  , genericError
  , genericDocError
  ) where

import Control.Monad ( unless )
import Control.Monad.State
import Control.Monad.IO.Class ( MonadIO )
import Data.Set ( Set )
import Data.Set qualified as Set
import Queue.Ephemeral ( EphemeralQueue(..) )
import Queue.Ephemeral qualified as Queue

import Agda.Compiler.Backend ( QName, Definition, getConstInfo, MonadDebug, reportSDoc, MonadTrace, ReadTCState, MonadTCError, TCErr )
import Agda.TypeChecking.Monad.Base ( TCM, MonadTCEnv, MonadTCM(liftTCM), MonadTCState, MonadTCEnv, HasOptions, genericError, genericDocError, internalError)
import Agda.Utils.List ( mcons )
import Agda.TypeChecking.Pretty
import Control.Monad.Error.Class (MonadError)

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
  deriving newtype (MonadIO, MonadFail, MonadDebug, ReadTCState, MonadTrace)
  deriving newtype (MonadError TCErr, MonadTCEnv, MonadTCState, HasOptions, MonadTCM)

-- | Require a definition to be compiled.
requireDef :: QName -> CompileM ()
requireDef q = Compile $ do
  seen <- gets seenDefs

  -- a name is only added to the queue if we haven't seen it yet
  unless (Set.member q seen) do

    reportSDoc "agda2lambox.compile.require" 10 $
      "Requiring definition:" <+> prettyTCM q

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
