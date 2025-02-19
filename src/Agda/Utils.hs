{-# LANGUAGE FlexibleContexts, OverloadedStrings, BangPatterns #-}

-- | Agda utilities.
module Agda.Utils where

import Control.Applicative ( liftA2 )
import Data.Bifunctor ( second )
import Data.Maybe ( isJust, isNothing )

import Agda.Compiler.Backend ( getUniqueCompilerPragma )
import Agda.Syntax.Abstract.Name
import Agda.TypeChecking.Monad.Base hiding ( conArity )
import Agda.TypeChecking.Datatypes ( getConstructorInfo, ConstructorInfo(..) )
import Agda.TypeChecking.Substitute ( raise )
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Treeless

import Agda.Compiler.ToTreeless          qualified as TT
import Agda.Compiler.Treeless.Builtin    qualified as TT
import Agda.Compiler.Treeless.Erase      qualified as TT
import Agda.Compiler.Treeless.Simplify   qualified as TT
import Agda.Compiler.Treeless.Identity   qualified as TT
import Agda.Compiler.Treeless.Uncase     qualified as TT
import Agda.Compiler.Treeless.AsPatterns qualified as TT


-- * Miscellaneous

pp :: Pretty a => a -> String
pp = prettyShow

hasPragma :: QName -> TCM Bool
hasPragma qn = isJust <$> getUniqueCompilerPragma "AGDA2LAMBOX" qn

isDataDef, isRecDef, isFunDef, isDataOrRecDef :: Defn -> Bool
isDataDef = \case
  Datatype{} -> True
  _ -> False

isRecDef = \case
  Record{} -> True
  _ -> False

isFunDef = \case
  Function{} -> True
  _ -> False

isDataOrRecDef = liftA2 (||) isDataDef isRecDef

-- ** toTreeless custom pipeline

-- | Convert compiled clauses to treeless syntax, and return it.
treeless :: QName -> TCM (Maybe TTerm)
treeless = TT.toTreelessWith compilerPipeline (EagerEvaluation, TT.EraseUnused)
  where
  compilerPipeline v q =
    TT.Sequential
      -- NOTE (flupe): this is the default Agda treeless pipeline
      --               with the builtin pass removed.
      -- [ TT.compilerPass "builtin" (30 + v) "builtin translation" $ const TT.translateBuiltins
      [ TT.FixedPoint 5 $ TT.Sequential
        [ TT.compilerPass "simpl"  (30 + v) "simplification"     $ const TT.simplifyTTerm
        , TT.compilerPass "erase"  (30 + v) "erasure"            $ TT.eraseTerms q
        , TT.compilerPass "uncase" (30 + v) "uncase"             $ const TT.caseToSeq
        , TT.compilerPass "aspat"  (30 + v) "@-pattern recovery" $ const TT.recoverAsPatterns
        ]
      , TT.compilerPass "id" (30 + v) "identity function detection" $ const (TT.detectIdentityFunctions q)
      ]

-- ** eta-expansion of constructors

-- NOTE(flupe):
--  We do this because CertiCoq requires fully-applied constructors.
--  In the future, this transformation should be made on the Coq side.
--  (as it would be proven correct).
--  NB: MetaCoq provides eta-expansion of constructors, but only for eprograms.
--  (see : erasure/theories/EConstructorsAsBlocks.v)

-- | Check whether a treeless term is a constructor applied to (many) terms.
unSpineCon :: TTerm -> Maybe (QName, [TTerm])
unSpineCon (TCon q)   = Just (q, [])
unSpineCon (TApp u v) = second (++ v) <$> unSpineCon u
unSpineCon _          = Nothing

-- | Return the arity of a constructor. Ignores parameters.
conArity :: ConstructorInfo -> Int
conArity (DataCon a) = a
conArity (RecordCon _ _ a _) = a

-- | Eta-expand treeless constructors.
etaExpandCtor :: TTerm -> TCM TTerm
etaExpandCtor t | Just (q, args) <- unSpineCon t = do
  arity <- conArity <$> getConstructorInfo q
  let nargs = length args

  if nargs >= arity then -- should really be (==), but just in case
    TApp (TCon q) <$> mapM etaExpandCtor args
  else do
    let nlam = arity - nargs
    exargs <- mapM (etaExpandCtor . raise nlam) args
    let vars = TVar <$> [(nlam - 1) .. 0]
    pure $ iterate TLam (TApp (TCon q) $ exargs ++ vars) !! nlam

etaExpandCtor t | otherwise = case t of
  TApp u v             -> TApp <$> etaExpandCtor u <*> mapM etaExpandCtor v
  TLam u               -> TLam <$> etaExpandCtor u
  TLet u v             -> TLet <$> etaExpandCtor u <*> etaExpandCtor v
  TCase k ci tdef alts -> TCase k ci <$> etaExpandCtor tdef 
                                     <*> mapM etaExpandAlt alts
  TCoerce u            -> TCoerce <$> etaExpandCtor u
  _                    -> pure t

etaExpandAlt :: TAlt -> TCM TAlt
etaExpandAlt = \case
  TACon q a b -> TACon q a <$> etaExpandCtor b
  TAGuard a b -> TAGuard a <$> etaExpandCtor b
  TALit l b   -> TALit l <$> etaExpandCtor b

  
-- * projections

isRecordProjection :: Defn -> Maybe (QName, QName)
isRecordProjection d
  | Function{..} <- d
  , Right Projection{..} <- funProjection
  , Just recName <- projProper
  = Just (recName, projOrig)
  | otherwise
  = Nothing


{-
lookupCtx :: MonadTCEnv m => Int -> m (String, Type)
lookupCtx i = first transcribe . (!! i) {-. reverse-} <$> currentCtx

currentCtxVars :: MonadTCEnv m => m [String]
currentCtxVars = fmap fst <$> currentCtx

lookupCtxVar :: MonadTCEnv m => Int -> m String
lookupCtxVar i = transcribe . (!! i) {-. reverse-} <$> currentCtxVars

currentCtxTys :: MonadTCEnv m => m [Type]
currentCtxTys = fmap snd <$> currentCtx

lookupCtxTy :: MonadTCEnv m => Int -> m Type
lookupCtxTy i = (!! i) {-. reverse-} <$> currentCtxTys

-- ** variables


qParent :: QName -> QName
qParent =
  qnameFromList . NE.fromList . reverse . drop 1 . reverse . qnameToList0

varPool :: String -> [String]
varPool varName = zipWith (<>) (repeat varName) $ "" : (show <$> [0..])

freshVarInCtx :: MonadIOEnv m => String -> m String
freshVarInCtx varName = freshVar varName' <$> currentCtxVars
  where
  varName' = if varName == "_" then "x" else varName

  freshVar :: String -> [String] -> String
  freshVar varName xs = head $ dropWhile (`elem` xs) (varPool varName)

freshVarsInCtx :: (MonadIOEnv m, MonadAddContext m) => Int -> m [String]
freshVarsInCtx n | n < 0 = error $ "[freshVarsInCtx] negative number of variables"
freshVarsInCtx 0 = return []
freshVarsInCtx n = do
  x <- freshVarInCtx "_"
  addContext [(x, defaultTy)] $
    (x :) <$> freshVarsInCtx (n - 1)

-- ** arguments & visibility

type TelItem = Dom (ArgName, Type)
type Tel     = [TelItem]

unElims :: [Elim] -> [Term]
unElims = fmap unArg . argsFromElims

hasQuantityNon0 :: LensQuantity a => a -> Bool
hasQuantityNon0 = not . hasQuantity0

shouldKeepTyParam :: PureTCM m => TelItem -> m (Maybe ArgName)
shouldKeepTyParam d@(unDom -> (x, ty)) = do
  isSrt <- isSortResTy ty
  return $ boolToMaybe (hasQuantityNon0 d && isSrt) x

shouldKeepTel :: PureTCM m => ListTel -> m ListTel
shouldKeepTel = filterM (fmap isNothing . shouldKeepTyParam)

shouldKeep :: (LensQuantity a, LensHiding a) => a -> Bool
shouldKeep = visible /\ hasQuantityNon0

shouldKeepRecField :: PureTCM m => TelItem -> m Bool
shouldKeepRecField d@(unDom -> (x, ty)) = do
  isLvl <- isLevelType ty
  return $ hasQuantityNon0 d && not isLvl

-- shouldKeepArgs :: [Arg a] -> [a]
-- shouldKeepArgs = fmap unArg . filter shouldKeep

-- shouldKeepElims :: [Elim] -> [Term]
-- shouldKeepElims = shouldKeepArgs . argsFromElims

isLevelTerm, isSortTerm :: Term -> Bool
isLevelTerm = \case
  Level _ -> True
  _       -> False
isSortTerm = isJust . isSort

isLevelTy, isSortTy :: Type -> Bool
isLevelTy = isLevelTerm . unEl
isSortTy  = isSortTerm  . unEl

returnTy :: Type -> Term
returnTy = flip (.) unEl $ \case
  Pi _ ty -> returnTy (unAbs ty)
  ty -> ty

isSortResTy :: PureTCM m => Type -> m Bool
isSortResTy ty = isSortTy <$> resTy ty

isSrtOrLvlTy :: PureTCM m => Type -> m Bool
isSrtOrLvlTy ty = (||) <$> isSortResTy ty <*> isLevelType ty

isSortM :: MonadTCEnv m => Term -> m Bool
isSortM = \case
  Sort _  -> return True
  Var n _ -> isSortTy <$> lookupCtxTy n
  _       -> return False

-- typeOf :: (MonadTCEnv m) => Term -> m Type
-- typeOf = \case
--   Var n _ -> typeOfBV n
--   Def n _ -> typeOfBV n

erasedArity :: Type -> Int
erasedArity t = case unEl t of
  Pi a b -> (if hasQuantity0 a then 0 else 1) + erasedArity (unAbs b)
  _      -> 0

-- ** treeless terms

defNameOfT :: TTerm -> Maybe QName
defNameOfT = \case
  TDef n -> Just n
  TCon n -> Just n
  -- TPrim n -> ??
  -- TVar n -> ??
  _ -> Nothing

-- | Perform multiple η-expansions on a treeless term.
etaExpandT :: Int -> Int -> Int -> TTerm -> TTerm
etaExpandT n k l t
  = mkTLam nk
  $ raise nk t `mkTApp` map (raise l . TVar) (downFrom n)
  where nk = max (n - k) 0 -- safeguard

-- ** erasure

data ClassifiedArg
  = TyParam ArgName
  | LvlParam
  | ErasedArg
  | KeptArg ArgName Type

isKeptArg, isTyParam :: ClassifiedArg -> Bool
isKeptArg = \case {KeptArg{} -> True; _ -> False}
isTyParam = \case {TyParam{} -> True; _ -> False}

classifyArg :: PureTCM m => TelItem -> m ClassifiedArg
classifyArg d@(unDom -> (x, ty)) = do
  isSrt <- isSortResTy ty
  isLvl <- isLevelType ty
  return $ if
    | isSrt -> TyParam x
    | isLvl -> LvlParam
    | hasQuantity0 d -> ErasedArg
    | otherwise -> KeptArg x ty

classifyArgs :: PureTCM m => [TelItem] -> m [ClassifiedArg]
classifyArgs = mapM classifyArg

isErasedTTerm :: TTerm -> Bool
isErasedTTerm = \case
  TErased -> True
  TSort   -> True
  _       -> False

isUnitTTerm :: TTerm -> Bool
isUnitTTerm = \case
  TUnit   -> True
  _       -> False

onlyNonErased :: [TTerm] -> [TTerm]
onlyNonErased = filter (not . isErasedTTerm)

isTyParamM :: PureEnvTCM m => TTerm -> m Bool
isTyParamM = \case
  TUnit -> return True
  TDef n -> isSortTy <$> typeOfConst n
  TVar i -> isSortTy <$> lookupCtxTy i
  TApp h _ -> do
    -- report $ "  t (head): " <> pp h
    isResTyParam h
    -- return False
    -- let ty = termFromTTerm tt
    -- resTy t
    where
      isResTyParam = \case
        TDef n -> isSortTy <$> (resTy =<< typeOfConst n)
        TVar i -> isSortTy <$> (resTy =<< lookupCtxTy i)
        _ -> return False
        -- t -> panic "result type parameter" t
  _ -> return False

isLevelM :: PureEnvTCM m => TTerm -> m Bool
isLevelM = \case
  TUnit -> return True
  TErased -> return True
  TDef n -> isLvl =<< typeOfConst n
  TVar i -> isLvl =<< lookupCtxTy i
  TApp h _ -> isResTyParam h
    where
    isResTyParam = \case
      TDef n -> isLvl =<< resTy =<< typeOfConst n
      TVar i -> isLvl =<< resTy =<< lookupCtxTy i
      _ -> return False
      -- t -> panic "result type parameter" t
  _ -> return False
  where
  isLvl ty = do
    isLvlTy <- isLevelType ty
    let isLvlUniv = pp ty == "Agda.Primitive.LevelUniv"
    return (isLvlTy || isLvlUniv)

separateTyParams :: PureEnvTCM m  => [TTerm] -> m ([TTerm], [TTerm])
separateTyParams = partitionM isTyParamM . filter (not . isErasedTTerm)

-- ** types & telescopes
isDependentArrow :: Dom Type -> Bool
isDependentArrow ty = pp (domName ty) `notElem` ["_", "(nothing)"]

typeFromTerm :: a -> Type'' Term a
typeFromTerm = El (DummyS "???" :: Sort)

elimFromTTerm :: TTerm -> Elim
elimFromTTerm = elimFromTerm . termFromTTerm

elimFromTerm :: Term -> Elim
elimFromTerm = Apply . defaultArg

termFromTTerm :: TTerm -> Term
termFromTTerm = \case
  TVar n -> Var n []
  TDef qn -> Def qn []
  TLit lit -> Lit lit
  -- TCon qn ->
  TApp t as -> let as' = elimFromTTerm <$> as in
    case termFromTTerm t of
      Var n [] -> Var n as'
      Def qn [] -> Def qn as'
      _ -> panic "treeless head" t
  TUnit -> Dummy "()" []
  t -> panic "tterm (to convert to term)" t

typeFromTTerm :: TTerm -> Type
typeFromTTerm = typeFromTerm . termFromTTerm

defaultTy :: Dom Type
defaultTy = defaultDom $ typeFromTerm (Dummy "???" [] :: Term)

telListView :: PureTCM m => Type -> m (ListTel, Type)
telListView t = do
  TelV tel t <- telView t -- telViewPath t
  return (telToList tel, t)

telListViewUpTo :: PureTCM m => Int -> Type -> m (ListTel, Type)
telListViewUpTo n t = do
  TelV tel t <- telViewUpTo n t
  return (telToList tel, t)

getArgTy :: PureTCM m => Type -> Int -> m Type
getArgTy ty i = do
  (tel, _) <- telListView ty
  return $ (snd . unDom <$> tel) !! i

viewTy :: PureTCM m => Type -> m (ListTel, ListTel, Type)
viewTy ty = do
  (tel, resTy) <- telListView ty
  -- let (vas, has) = partition shouldKeep tel
  let (vas, has) = partition visible tel
  -- (has, vas) <- partitionM (fmap isKeptArg . classifyArg) tel
  return (has, vas, resTy)

argTys :: PureTCM m => Type -> m ListTel
argTys ty = fst <$> telListView ty

vargTys :: PureTCM m => Type -> m ListTel
vargTys ty = do
  (_ , vas, _) <- viewTy ty
  return vas

resTy :: PureTCM m => Type -> m Type
resTy ty = do
  (_ , _, ty) <- viewTy ty
  return ty

isNullary :: PureTCM m => Type -> m Bool
isNullary ty = null . filter hasQuantityNon0 . fst <$> telListView ty

-- filterTel :: (Dom Type -> Bool) -> Telescope -> Telescope
-- filterTel p = \case
--   EmptyTel -> EmptyTel
--   ExtendTel a tel
--     (if p a then ExtendTel
--     | ->
--     | otherwise -> traverseF (filterTel p) tel

-- ** definitions


funCC :: (MonadTCM m, HasConstInfo m) => QName -> m CompiledClauses
funCC q = do
  def <- theDef <$> getConstInfo q
  case def of
    Function{..} -> do
      case funCompiled of
        Just cc -> return cc
        Nothing -> panic "function clauses (not compiled yet)" def
    _ -> panic "definition (not a function)" def

-}
