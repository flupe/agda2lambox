{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Compile.Type
  ( compileType
  , compileTele
  ) where


import Control.Monad.Reader
import Control.Monad ( mapM )
import Data.List ( foldl' )
import Data.Function ( (&) )

import Agda.Syntax.Common ( unArg )
import Agda.Syntax.Internal
import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.Syntax.Common.Pretty ( prettyShow )

import qualified LambdaBox as LBox
import Agda2Lambox.Compile.Utils ( qnameToKName )
import Agda2Lambox.Compile.Monad

-- NOTE(flupe):
--   strategy for type compilation (for future me)
--   like for terms, we need to keep track of bound variables.
--   In particular, in λ□ type syntax, variables refer to type variables, using DeBruijn *levels*.
--   So, when compiling var k,
--     we should check that if k is below the amount of locally bound bars.
--       If such => tBox.
--       otherwise, it HAS to point to a type variable, and we 
--   

-- | λ□ type compilation environment.
data CompileEnv = CompileEnv
  { typeVars  :: Int
    -- ^ Type variables, bound outside of the type.
  , boundVars :: Int
    -- ^ Amount of locally-bound variables.
  }

initEnv :: Int -> CompileEnv
initEnv tvs = CompileEnv
  { typeVars  = tvs
  , boundVars = 0
  }

runC :: Int -> C a -> CompileM a
runC tvs m = runReaderT m (initEnv tvs)

-- | Increment the number of locally-bound variables.
underBinder :: C a -> C a
underBinder = local \e -> e { boundVars = boundVars e + 1 }

-- | Compilation monad.
type C a = ReaderT CompileEnv CompileM a


compileTopLevelType :: Type -> CompileM (LBox.Type)
compileTopLevelType = undefined

-- | Compile a type, given a number of type variables in scope.
compileType :: Type -> CompileM LBox.Type
compileType = runC 0 . compileType'

compileType' :: Type -> C LBox.Type
compileType' = compileType'' . unEl

compileType'' :: Term -> C LBox.Type
compileType'' = \case
  Var n es -> do
    CompileEnv{..} <- ask
    if n < boundVars then
      pure LBox.TBox -- NOTE(flupe): should we still apply the parameters to the box?
    else do
      let k = typeVars - (n - boundVars)
      -- NOTE(flupe): reading the paper, type variables are restricted to Hindley-Milner
      --              so cannot be type constructors: we don't compile elims
      pure $ LBox.TVar k
      -- foldl' LBox.TApp (LBox.TVar k) <$> compileElims es

  Def q es -> do
    -- TODO(flupe): check if it's an inductive, or a type alias
    foldl' LBox.TApp (LBox.TConst $ qnameToKName q) <$> compileElims es
  Pi dom abs ->
    LBox.TArr <$> compileType' (unDom dom)
              <*> underBinder (compileType' (unAbs abs))

  -- NOTE(flupe):
  --  My current understanding of typed lambox is that the type translation 
  t       -> pure LBox.TBox

compileElims :: Elims -> C [LBox.Type]
compileElims = mapM \case
  Apply a  -> compileType'' $ unArg a
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

