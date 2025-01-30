{-# LANGUAGE DerivingVia, GeneralizedNewtypeDeriving, OverloadedStrings, DeriveFunctor, DeriveTraversable #-}
-- | Compilation monad.
module Agda2Lambox.Compile.Monad
  ( CompileM
  , requireDef
  , compileLoop
  , genericError
  , genericDocError
  , internalError
  , CompiledItem(..)
  ) where

import Control.Monad ( unless )
import Control.Monad.State
import Control.Monad.IO.Class ( MonadIO )
import Data.Set ( Set )
import Data.Set qualified as Set
import Queue.Ephemeral ( EphemeralQueue(..) )
import Queue.Ephemeral qualified as Queue

import Agda.Syntax.Abstract (QName)
import Agda.Compiler.Backend (getConstInfo, PureTCM, HasConstInfo, HasBuiltins, canonicalName)
import Agda.TypeChecking.Monad (MonadDebug, MonadTrace, MonadAddContext)
import Agda.TypeChecking.Monad.Debug (MonadDebug, reportSDoc)
import Agda.TypeChecking.Monad.Base hiding (initState)
import Agda.Utils.List ( mcons )
import Agda.TypeChecking.Pretty
import Control.Monad.Error.Class (MonadError)

-- | Backend compilation state.
data CompileState = CompileState
  { seenDefs     :: Set QName
    -- ^ Names that we have seen, either already compiled or in the queue.
  , compileQueue :: EphemeralQueue QName
    -- ^ Compilation queue.
  , requiredDefs :: Set QName
    -- ^ (Locally) required definitions.
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
  , requiredDefs = Set.empty
  }

-- | Backend compilation monad.
newtype CompileM a = Compile (StateT CompileState TCM a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadIO, MonadFail, MonadDebug, ReadTCState, MonadTrace)
  deriving newtype (MonadError TCErr, MonadTCEnv, MonadTCState, HasOptions, MonadTCM)
  deriving newtype (MonadAddContext, MonadReduce, HasConstInfo, HasBuiltins, PureTCM)

-- | Require a definition to be compiled.
requireDef :: QName -> CompileM ()
requireDef q = Compile $ do
  q <- liftTCM $ canonicalName q


  -- add name to the required list
  modify \ s -> s { requiredDefs = Set.insert q (requiredDefs s) }

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

data CompiledItem a = CompiledItem
  { itemName  :: QName
  , itemDeps  :: [QName]
  , itemValue :: a
  } deriving (Functor, Foldable, Traversable)

-- | Record the required definitions of a compilation unit.
trackDeps :: CompileM a -> CompileM (a, [QName])
trackDeps (Compile c) = Compile do
  modify \s -> s {requiredDefs = Set.empty}
  x <- c
  deps <- gets requiredDefs
  pure (x, Set.toList deps)

-- | Run the processing function as long as there are names to be compiled.
compileLoop
  :: forall a. (Definition -> CompileM a) -- ^ The compilation function
  -> [QName]                            -- ^ Names to compile
  -> TCM [CompiledItem a]
compileLoop step = evalStateT unloop . initState
  where
  loop :: CompileM [CompiledItem a]
  loop@(Compile unloop) = nextUnit >>= \case
    Nothing -> pure []
    Just q  -> do
      (mr, deps) <- trackDeps . step =<< (liftTCM $ getConstInfo q)
      let item = CompiledItem 
              { itemName  = q
              , itemDeps  = deps
              , itemValue = mr
              }
      (item:) <$> loop
