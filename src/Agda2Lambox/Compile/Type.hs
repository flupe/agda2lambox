{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Compile.Type
  ( compileType
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

  Lit{}   -> fail "type-level literals not supported."
  Lam{}   -> fail "type-level abstractions not supported."
  Con{}   -> fail "type-level constructors not supported."
  Sort{}  -> pure LBox.TBox
  Level{} -> pure LBox.TBox
  t       -> fail $ "unsupported type: " <> prettyShow t


compileElims :: Elims -> TCM [LBox.Type]
compileElims = mapM \case
  Apply a  -> compileType' $ unArg a
  Proj{}   -> fail "type-level projection elim not supported."
  IApply{} -> fail "type-level cubical path application not supported."
