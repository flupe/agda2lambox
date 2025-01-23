{-# LANGUAGE LambdaCase, FlexibleInstances, MultiWayIf #-}
module Agda2Lambox.Compile.Type
  ( compileType
  , compileTopLevelType
  , compileArgs
  , compileTele
  ) where


import Control.Category ((>>>))
import Control.Monad.Reader
import Control.Monad ( mapM )
import Data.List ( foldl' )
import Data.Function ( (&) )
import Data.Bifunctor ( second )
import Data.Maybe ( isJust )

import Agda.Syntax.Common ( unArg )
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base ( TCM, MonadTCM (liftTCM), Definition (theDef) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Utils.Monad (ifM)

import qualified LambdaBox as LBox
import Agda2Lambox.Compile.Utils ( qnameToKName )
import Agda2Lambox.Compile.Monad
import Agda.Compiler.Backend (HasConstInfo(getConstInfo))
import Agda.Utils (isDataOrRecDef, getInductiveParams, isLogical, isArity)
import Agda.TypeChecking.Substitute (absBody, TelV (theCore))
import Agda.TypeChecking.Telescope (telView)

-- | The kind of variables that are locally bound
data VarType = TypeVar Int | IndVar | Other

-- | λ□ type compilation environment.
data CompileEnv = CompileEnv
  { typeVars   :: Int
    -- ^ How many type variables we have.
  , boundVars  :: Int
    -- ^ Amount of bound variables (including type variables)
  , boundTypes :: [VarType]
    -- ^ Information about the type variables in question.
    -- Should be indexed using de Bruijn indices.
  }

-- | Initialize compilation environment with a given number of type variables.
initEnv :: Int -> CompileEnv
initEnv tvs = CompileEnv
  { typeVars   = tvs
  , boundVars  = tvs
  , boundTypes = reverse $ TypeVar <$> [0 .. tvs - 1]
  }

runC :: Int -> C a -> CompileM a
runC tvs m = runReaderT m (initEnv tvs)

-- | Increment the number of locally-bound variables.
underBinder :: VarType -> C a -> C a
underBinder v = local \e -> e
  { boundVars  = boundVars e + 1
  , boundTypes = v : boundTypes e
  }

underTypeVar :: C a -> C a
underTypeVar = local \CompileEnv{..} -> CompileEnv
  { typeVars   = typeVars + 1
  , boundVars  = boundVars + 1
  , boundTypes = TypeVar typeVars : boundTypes
  }

-- | Type compilation monad.
type C a = ReaderT CompileEnv CompileM a


-- | Compile constructor arguments' types, given a set number of type variables.
compileArgs :: Int -> [Dom Type] -> CompileM [(LBox.Name, LBox.Type)]
compileArgs tvars = runC tvars . compileArgsC
  where
  compileArgsC :: [Dom Type] -> C [(LBox.Name, LBox.Type)]
  compileArgsC [] = pure []
  compileArgsC (dom:args) = do
    let name = maybe LBox.Anon (LBox.Named . prettyShow) $ domName dom
    typ <- compileTypeC $ unDom dom
    -- TODO(flupe): pick the right var type
    ((name, typ):) <$> underBinder Other (compileArgsC args)


-- | Compile a top-level type to λ□, lifting out type variables.
--
-- The algorithm is a slight variation over the one presented
-- in [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995).

compileTopLevelType :: Type -> CompileM ([LBox.Name], LBox.Type)
compileTopLevelType = runC 0 . compileTopLevelTypeC
  where
  compileTopLevelTypeC :: Type -> C ([LBox.Name], LBox.Type)
  compileTopLevelTypeC typ =
    ifM (liftTCM $ isLogical typ) (pure ([], LBox.TBox)) $
    case unEl typ of

      Pi dom codom -> do

        let domType      = unDom dom
        let codomType    = absBody codom

        domIsLogical <- liftTCM $ isLogical domType
        domIsArity   <- liftTCM $ isArity domType

        -- logical types and non-arities can never be lifted to type variables,
        -- we keep going
        if domIsLogical || not domIsArity then do
          domType' <- if domIsLogical then pure LBox.TBox
                                      else compileTypeC domType
          (vars, codomType')
            <- underBinder Other $ compileTopLevelTypeC codomType

          pure (vars, LBox.TArr domType' codomType')

        else do
          (vars, codomType') <- underTypeVar $ compileTopLevelTypeC codomType
          let name = maybe LBox.Anon (LBox.Named . prettyShow) $ domName dom
          pure (name : vars, LBox.TArr LBox.TBox codomType')

      t -> ([],) <$> compileTypeTermC t


-- | Compile a type, given a number of type variables in scope.
compileType :: Int -> Type -> CompileM LBox.Type
compileType tvars = runC tvars . compileTypeC


compileTypeC :: Type -> C LBox.Type
compileTypeC = compileTypeTermC . unEl

-- Apply a type constructor to a list of types.
tapp :: LBox.Type -> [LBox.Type] -> LBox.Type
tapp = foldl' LBox.TApp

compileTypeTermC :: Term -> C LBox.Type
compileTypeTermC = unSpine >>> \case
  Sort{} -> pure LBox.TBox

  Var n _ -> do
    (!! n) <$> asks boundTypes >>= \case
      TypeVar n -> pure $ LBox.TVar n
      Other     -> pure $ LBox.TAny

  -- agda definition applied to some arguments
  Def q es -> do
    def <- liftTCM $ theDef <$> getConstInfo q

    -- if it's an inductive:
    if isDataOrRecDef def then
      -- we only apply the parameters
      tapp (LBox.TConst $ qnameToKName q)
        <$> compileElims (take (getInductiveParams def) es)

    -- TODO: check if it is a type alias
    --   (if it is, do more or less the same thing as above)

    -- else: it's any! (or a box?)
    else pure LBox.TAny

  Pi dom abs -> do
    let domType   = unDom dom
    let codomType = absBody abs

    domIsLogical <- liftTCM $ isLogical domType
    domIsArity   <- liftTCM $ isArity domType

    -- logical types and non-arities can never be lifted to type variables,
    -- we keep going
    domType' <-
      if | domIsLogical || domIsArity -> pure LBox.TBox
         | otherwise -> compileTypeTermC $ unEl domType

    codomType'
      <- underBinder Other $ compileTypeTermC $ unEl $ codomType

    pure $ LBox.TArr domType' codomType'

  -- NOTE(flupe):
  --  My current understanding of typed lambox is that the type translation never fails.
  --  worst case, we cannot generate nice types and fall back on TAny
  t -> pure LBox.TAny

compileElims :: Elims -> C [LBox.Type]
compileElims = mapM \case
  Apply a  -> compileTypeTermC $ unArg a
  Proj{}   -> genericError "type-level projection elim not supported."
  IApply{} -> genericError "type-level cubical path application not supported."

-- See: https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/Erasure.v#L780-L817
-- | Compile a telescope (of parameters) into a list of λ□ type variables.
compileTele :: Tele (Dom Type) -> TCM [LBox.TypeVarInfo]
compileTele tel =
  telToList tel
  & map unDom
  & traverse \(argname, t) -> do
    undefined
    pure LBox.TypeVarInfo
      { tvarName      = LBox.Named argname
      , tvarIsLogical = False
          -- ^ type is logical if it is "a proposition when fully applied"
          --    i.e t       : Prop =>  t is an arity
          --        t a₁ a₂ : Prop =>  t is an arity
      , tvarIsArity   = False
          -- ^ type t is an arity if it is "an n-ary dependent function ending with a sort"
      , tvarIsSort    = False
          -- ^ if the type of the parameter ends in a sort.
          --     say, @(T : Type)@ or @(T : nat -> Type)@ or @(T : Type -> Type)@.
      }

