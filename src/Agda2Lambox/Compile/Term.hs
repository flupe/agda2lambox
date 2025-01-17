{-# LANGUAGE NamedFieldPuns, DerivingVia, GeneralizedNewtypeDeriving #-}
module Agda2Lambox.Compile.Term
  ( compileTerm
  ) where

import Control.Monad ( mapM )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class ( MonadReader, ask )
import Control.Monad.Reader ( ReaderT(runReaderT), local )
import Data.List ( elemIndex, foldl' )
import Data.Maybe ( fromMaybe, listToMaybe )

import Agda (TCM , liftTCM, MonadTCEnv, MonadTCM)
import Agda.Compiler.Backend ( MonadTCState, HasOptions )
import Agda.Compiler.Backend ( getConstInfo, theDef, pattern Datatype, dataMutual )
import Agda.Syntax.Abstract.Name ( ModuleName(..), QName(..) )
import Agda.Syntax.Common ( Erased(..) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Treeless ( TTerm(..), TAlt(..), CaseInfo(..), CaseType(..) )
import Agda.TypeChecking.Datatypes ( getConstructorData, getConstructors )

import LambdaBox ( Term(..) )
import LambdaBox qualified as LBox
import Agda2Lambox.Compile.Utils


-- * Term compilation monad

-- | λ□ compilation environment.
data CompileEnv = CompileEnv
  { mutuals   :: [QName]
    -- ^ When we compile mutual function definitions,
    -- they are introduced at the top of the local context.
  , boundVars :: Int
    -- ^ Amount of locally-bound variables.
  }

-- | Initial compilation environment.
-- No mutuals, no bound variables.
initEnv :: CompileEnv
initEnv = CompileEnv
  { mutuals   = []
  , boundVars = 0
  }

-- | Compilation monad.
newtype C a = C (ReaderT CompileEnv TCM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving newtype (MonadFail, MonadReader CompileEnv)
  deriving newtype (MonadTCEnv, MonadTCState, HasOptions, MonadTCM)

-- | Run a compilation unit in @TCM@, in the initial environment.
runC :: C a -> TCM a
runC (C m) = runReaderT m initEnv

-- | Increase the number of locally-bound variables.
underBinders :: Int -> C a -> C a
underBinders n = local \e -> e { boundVars = boundVars e + n }

-- | Increment the number of locally-bound variables.
underBinder :: C a -> C a
underBinder = underBinders 1
{-# INLINE underBinder #-}

-- | Set local mutual fixpoints.
withMutuals :: [QName] -> C a -> C a
withMutuals ms = local \e -> e { mutuals = reverse ms }


-- * Term conversion

-- | Convert a treeless term to its λ□ equivalent.
compileTerm
  :: [QName]
     -- ^ Local fixpoints.
  -> TTerm
  -> TCM LBox.Term
compileTerm ms = runC . withMutuals ms . compileTermC

-- | Convert a treeless term to its λ□ equivalent.
compileTermC :: TTerm -> C LBox.Term
compileTermC = \case
  TVar  n -> pure $ LRel n
  TPrim p -> fail "primitives not supported"

  TDef qn -> do
    CompileEnv{mutuals, boundVars} <- ask
    return case qn `elemIndex` mutuals of
      Nothing -> LConst $ qnameToKName qn
      Just i  -> LRel   $ i + boundVars

  TCon q             -> liftTCM $ toConApp q []
  TApp (TCon q) args -> liftTCM . toConApp q =<< mapM compileTermC args
  -- ^ For dealing with fully-applied constructors

  TApp u es -> do
    cu  <- compileTermC u
    ces <- mapM compileTermC es
    pure $ foldl' LApp cu ces

  TLam t -> underBinder $ LLam <$> compileTermC t

  TLit l -> fail "literals not supported"

  TLet u v -> LLet <$> compileTermC u
                   <*> underBinder (compileTermC v)

  TCase n CaseInfo{..} dt talts ->
    case caseErased of
      Erased _    -> fail "Erased matches not supported."
      NotErased _ -> do
        cind  <- liftTCM $ compileCaseType caseType
        LCase cind 0 (LRel n) <$> mapM compileAlt talts

  TUnit   -> return LBox
  TSort   -> return LBox
  TErased -> return LBox

  TCoerce tt  -> fail "Coerces not supported."
  TError terr -> fail "Errors not supported."


compileCaseType :: CaseType -> TCM LBox.Inductive
compileCaseType = \case
  CTData qn -> toInductive qn
  _         -> fail "case type not supported"


compileAlt :: TAlt -> C ([LBox.Name], LBox.Term)
compileAlt = \case
  TACon{..}   -> let names = take aArity $ repeat LBox.Anon
                 in (names,) <$> underBinders aArity (compileTermC aBody)
  TALit{..}   -> fail "literal patterns not supported"
  TAGuard{..} -> fail "case guards not supported"
