{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Compile.Type
  ( compileType
  , compileTele
  ) where


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


compileType :: Type -> TCM LBox.Type
compileType = compileType' . unEl


compileType' :: Term -> TCM LBox.Type
compileType' = \case
  Var n es -> foldl' LBox.TApp (LBox.TVar n) <$> compileElims es
  Def q es -> do
    -- TODO(flupe): check if it's an inductive
    foldl' LBox.TApp (LBox.TConst $ qnameToKName q) <$> compileElims es
  Pi dom abs ->
    LBox.TArr <$> compileType (unDom dom)
              <*> compileType (unAbs abs)

  Lit{}   -> genericError "type-level literals not supported."
  Lam{}   -> genericError "type-level abstractions not supported."
  Con{}   -> genericError "type-level constructors not supported."
  Sort{}  -> pure LBox.TBox
  Level{} -> pure LBox.TBox
  t       -> genericError $ "unsupported type: " <> prettyShow t

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
          -- ^ t : Prop?
      }


compileElims :: Elims -> TCM [LBox.Type]
compileElims = mapM \case
  Apply a  -> compileType' $ unArg a
  Proj{}   -> genericError "type-level projection elim not supported."
  IApply{} -> genericError "type-level cubical path application not supported."
