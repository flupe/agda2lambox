{-# LANGUAGE NamedFieldPuns, DerivingVia, GeneralizedNewtypeDeriving #-}
module Agda2Lambox.Compile.Term
  ( compileTerm
  ) where

import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class ( MonadReader, ask )
import Control.Monad.Reader ( ReaderT(runReaderT), local )
import Control.Monad.Trans
import Data.List ( elemIndex, foldl', singleton )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Foldable ( foldrM )

import Agda.Compiler.Backend ( MonadTCState, HasOptions )
import Agda.Compiler.Backend ( getConstInfo, theDef, pattern Datatype, dataMutual )
import Agda.Syntax.Abstract.Name ( ModuleName(..), QName(..) )
import Agda.Syntax.Builtin ( builtinZero, builtinSuc )
import Agda.Syntax.Common ( Erased(..) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Literal
import Agda.Syntax.Treeless ( TTerm(..), TAlt(..), CaseInfo(..), CaseType(..) )
import Agda.TypeChecking.Datatypes ( getConstructorData, getConstructors )
import Agda.TypeChecking.Monad.Base ( TCM , liftTCM, MonadTCEnv, MonadTCM )
import Agda.TypeChecking.Monad.Builtin ( getBuiltinName_ )

import LambdaBox ( Term(..) )
import LambdaBox qualified as LBox
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad ( CompileM, requireDef )


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
type C a = ReaderT CompileEnv CompileM a

-- | Run a compilation unit in @TCM@, in the initial environment.
runC :: C a -> CompileM a
runC m = runReaderT m initEnv

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
  -> CompileM LBox.Term
compileTerm ms = runC . withMutuals ms . compileTermC

-- | Convert a treeless term to its λ□ equivalent.
compileTermC :: TTerm -> C LBox.Term
compileTermC = \case
  TVar  n -> pure $ LRel n
  TPrim p -> fail "primitives not supported"

  TDef qn -> do
    CompileEnv{mutuals, boundVars} <- ask
    case qn `elemIndex` mutuals of
      Nothing -> do lift $ requireDef qn; pure $ LConst $ qnameToKName qn
      Just i  -> pure $ LRel  $ i + boundVars

  TCon q -> do
    lift $ requireDef q
    liftTCM $ toConApp q []

  TApp (TCon q) args -> do
    lift $ requireDef q
    liftTCM . toConApp q =<< traverse compileTermC args
  -- ^ For dealing with fully-applied constructors

  TApp u es -> do
    cu  <- compileTermC u
    ces <- traverse compileTermC es
    pure $ foldl' LApp cu ces

  TLam t -> underBinder $ LLam <$> compileTermC t

  TLit l -> compileLit l

  TLet u v -> LLet <$> compileTermC u
                   <*> underBinder (compileTermC v)

  TCase n CaseInfo{..} dt talts ->
    case caseErased of
      Erased _    -> fail "Erased matches not supported."
      NotErased _ -> do
        cind  <- compileCaseType caseType
        LCase cind 0 (LRel n) <$> traverse compileAlt talts

  TUnit   -> return LBox
  TSort   -> return LBox
  TErased -> return LBox

  TCoerce tt  -> fail "Coerces not supported."
  TError terr -> fail "Errors not supported."

compileLit :: Literal -> C LBox.Term
compileLit = \case

  LitNat i -> do
    qz <- liftTCM $ getBuiltinName_ builtinZero
    qs <- liftTCM $ getBuiltinName_ builtinSuc
    z  <- liftTCM $ toConApp qz []
    let ss = take (fromInteger i) $ repeat (toConApp qs . singleton)
    liftTCM $ foldrM ($) z ss

  l -> fail $ "unsupported literal: " <> prettyShow l

compileCaseType :: CaseType -> C LBox.Inductive
compileCaseType = \case
  CTData qn -> do lift $ requireDef qn
                  liftTCM $ toInductive qn
  _         -> fail "case type not supported"


compileAlt :: TAlt -> C ([LBox.Name], LBox.Term)
compileAlt = \case
  TACon{..}   -> let names = take aArity $ repeat LBox.Anon
                 in (names,) <$> underBinders aArity (compileTermC aBody)
  TALit{..}   -> fail "literal patterns not supported"
  TAGuard{..} -> fail "case guards not supported"
