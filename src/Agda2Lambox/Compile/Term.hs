{-# LANGUAGE NamedFieldPuns, DerivingVia, OverloadedStrings #-}
module Agda2Lambox.Compile.Term
  ( compileTerm
  ) where

import Control.Monad.Fail ( MonadFail )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class ( MonadReader, ask, asks )
import Control.Monad.Reader ( ReaderT(runReaderT), local )
import Control.Monad.Trans
import Data.List ( elemIndex, foldl', singleton )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Foldable ( foldrM )

import Agda.Compiler.Backend ( MonadTCState, HasOptions )
import Agda.Compiler.Backend ( getConstInfo, theDef, pattern Datatype, dataMutual, reportSDoc )
import Agda.Syntax.Abstract.Name ( ModuleName(..), QName(..) )
import Agda.Syntax.Builtin ( builtinNat, builtinZero, builtinSuc )
import Agda.Syntax.Common ( Erased(..) )
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Literal
import Agda.Syntax.Treeless
  ( TTerm(..), TAlt(..), CaseInfo(..), CaseType(..), TPrim(..) )
import Agda.TypeChecking.Datatypes ( getConstructorData, getConstructors )
import Agda.TypeChecking.Monad.Base ( TCM , liftTCM, MonadTCEnv, MonadTCM )
import Agda.TypeChecking.Monad.Builtin ( getBuiltinName_ )

import LambdaBox.Names ( Name(..) )
import LambdaBox ( Term(..) )
import LambdaBox qualified as LBox
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda.TypeChecking.Pretty (PrettyTCM(prettyTCM))


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

  TVar  n -> do
    bound <- asks boundVars
    if n < bound then pure $ LRel n
                 else pure $ LBox -- a type variable

  TPrim p -> do
    reportSDoc "agda2lambox.compile.term" 5 $ "primitives not supported: " <> prettyTCM (show p)
    pure LBox

  -- NOTE(flupe):
  -- Assumption:
  --   the only Defs remaining after the treeless translation
  --   are computationally relevant.
  --   - they cannot be propositions.
  --   - they cannot be types.
  TDef qn -> do
    CompileEnv{mutuals, boundVars} <- ask
    case qn `elemIndex` mutuals of
      Nothing -> do lift $ requireDef qn
                    pure $ LConst $ qnameToKName qn
      Just i  -> pure $ LRel  $ i + boundVars

  TCon q -> do
    lift $ requireDef q
    liftTCM $ toConApp q []

  -- TODO: maybe not ignore seq? (c.f. issue #12)
  TApp (TPrim PSeq) args -> compileTermC (last args)

  TApp (TCon q) args -> do
    lift $ requireDef q
    traverse compileTermC args
      >>= liftTCM . toConApp q
  -- ^ For dealing with fully-applied constructors

  TApp u es -> do
    cu  <- compileTermC u
    ces <- traverse compileTermC es
    pure $ foldl' LApp cu ces

  TLam t -> underBinder $ LLambda Anon <$> compileTermC t

  TLit l -> compileLit l

  TLet u v -> LLetIn Anon <$> compileTermC u
                          <*> underBinder (compileTermC v)

  TCase n CaseInfo{..} dt talts ->
    case caseErased of
      Erased _    -> genericError "Erased matches not supported."
      NotErased _ -> do
        cind  <- compileCaseType caseType
        LCase cind 0 (LRel n) <$> traverse compileAlt talts

  TUnit   -> return LBox
  TSort   -> return LBox
  TErased -> return LBox

  TError terr -> return LBox -- unreachable clause
  TCoerce tt  -> genericError "Coerces not supported."

compileLit :: Literal -> C LBox.Term
compileLit = \case

  LitNat i -> do
    qn <- liftTCM $ getBuiltinName_ builtinNat
    qz <- liftTCM $ getBuiltinName_ builtinZero
    qs <- liftTCM $ getBuiltinName_ builtinSuc
    z  <- liftTCM $ toConApp qz []
    let ss = take (fromInteger i) $ repeat (toConApp qs . singleton)
    lift $ requireDef qn
    liftTCM $ foldrM ($) z ss

  l -> genericError $ "unsupported literal: " <> prettyShow l

compileLitPattern :: Literal -> C LBox.Term
compileLitPattern = \case

  LitNat i -> do
    qn <- liftTCM $ getBuiltinName_ builtinNat
    qz <- liftTCM $ getBuiltinName_ builtinZero
    qs <- liftTCM $ getBuiltinName_ builtinSuc
    z  <- liftTCM $ toConApp qz []
    let ss = take (fromInteger i) $ repeat (toConApp qs . singleton)
    lift $ requireDef qn
    liftTCM $ foldrM ($) z ss

  l -> genericError $ "unsupported literal: " <> prettyShow l

compileCaseType :: CaseType -> C LBox.Inductive
compileCaseType = \case
  CTData qn -> do
    lift $ requireDef qn
    liftTCM $ toInductive qn
  CTNat -> do
    qn <- liftTCM $ getBuiltinName_ builtinNat
    lift $ requireDef qn
    liftTCM $ toInductive qn
  ct -> genericError $ "Case type not supported: " <> show ct

-- TODO: flupe
--   literal patterns are a bit annoying.
--   3 -> body
--   should get compiled to:
--   succ n -> case n of
--     succ n -> case n of
--       succ n -> case n of
--         zero -> body
--   but how do we handle the generation of other branches?
--   perhaps there's already a treeless translation to prevent this
--   to inverstigate...

compileAlt :: TAlt -> C ([LBox.Name], LBox.Term)
compileAlt = \case
  TACon{..}   -> let names = take aArity $ repeat LBox.Anon
                 in (names,) <$> underBinders aArity (compileTermC aBody)

  -- hardcoded support for zero nat literal pattern
  TALit (LitNat 0) body -> do
    qn <- liftTCM $ getBuiltinName_ builtinNat
    lift $ requireDef qn
    ([],) <$> compileTermC body

  lit@TALit{..} -> genericError $ "Literal pattern not supported:" <> show lit
  TAGuard{..}   -> genericError "case guards not supported"
