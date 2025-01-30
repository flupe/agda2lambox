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

import Agda.Syntax.Abstract (QName)
import Agda.Compiler.Backend (getConstInfo, PureTCM, HasConstInfo, HasBuiltins, canonicalName)
import Agda.TypeChecking.Monad (MonadDebug, MonadTrace, MonadAddContext)
import Agda.TypeChecking.Monad.Debug (MonadDebug, reportSDoc)
import Agda.TypeChecking.Monad.Base hiding (initState)
import Agda.Utils.List ( mcons )
import Agda.TypeChecking.Pretty
import Control.Monad.Error.Class (MonadError)

import Agda2Lambox.Compile.Utils (CompiledItem(..), topoSort)

-- | Backend compilation state.
data CompileState = CompileState
  { seenDefs     :: Set QName
    -- ^ Names that we have seen, either already compiled or in the stack.
  , compileStack :: [QName]
    -- ^ Compilation stack.
  , requiredDefs :: Set QName
    -- ^ (Locally) required definitions.
  }

-- | Initial compile state, with a set of names required for compilation.
initState :: [QName] -> CompileState
initState qs = CompileState
  { seenDefs     = Set.fromList qs
  , compileStack = qs
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
      , compileStack = q : compileStack s
      }

-- | Get the next qname to be compiled, if there is one.
nextUnit :: CompileM (Maybe QName)
nextUnit = Compile $
  gets compileStack >>= \case
    []   -> pure Nothing
    q:qs -> Just q <$ modify \ s -> s {compileStack = qs}


-- | Record the required definitions of a compilation unit.
trackDeps :: CompileM a -> CompileM (a, [QName])
trackDeps (Compile c) = Compile do
  modify \s -> s {requiredDefs = Set.empty}
  x <- c
  deps <- gets requiredDefs
  pure (x, Set.toList deps)

-- | Run the processing function as long as there are names to be compiled.
--  Returns a list of compiled items, (topologically) sorted by dependency order.
--  This means that whenever @A@ depends on @B@, @A@ will appear before @B@ in the list.
compileLoop
  :: forall a. (Definition -> CompileM (Maybe a))
       -- ^ The compilation function
  -> [QName] -- ^ Initial names to compile
  -> TCM [CompiledItem a]
compileLoop step names = topoSort <$> evalStateT unloop (initState names)
  where
  loop :: CompileM [CompiledItem (Maybe a)]
  loop@(Compile unloop) = nextUnit >>= \case
    Nothing -> pure []
    Just q  -> do
      (mr, deps) <- trackDeps . step =<< (liftTCM $ getConstInfo q)
      (CompiledItem q deps mr:) <$> loop
