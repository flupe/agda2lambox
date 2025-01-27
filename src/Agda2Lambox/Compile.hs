{-# LANGUAGE NamedFieldPuns, DataKinds, OverloadedStrings, NondecreasingIndentation #-}
module Agda2Lambox.Compile 
  ( compile
  ) where

import Control.Monad.IO.Class ( liftIO )
import Data.IORef

import Agda.Compiler.Backend
import Agda.Syntax.Internal ( QName )
import Agda.Syntax.Common (Arg(..))
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Monad ( liftTCM, getConstInfo )
import Agda.TypeChecking.Pretty
import Agda.Utils.Monad ( whenM, ifM )

import Agda.Utils ( hasPragma, isDataOrRecDef, treeless )

import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Term      ( compileTerm )
import Agda2Lambox.Compile.Function  ( compileFunction  )
import Agda2Lambox.Compile.Inductive ( compileInductive )

import LambdaBox.Names
import LambdaBox.Env (GlobalEnv(..), GlobalDecl(..), ConstantBody(..))
import LambdaBox.Type qualified as LamBox

import Agda2Lambox.Compile.Type (compileTopLevelType)

-- | Compile the given names to a λ□ environment.
compile :: Target t -> [QName] -> TCM (GlobalEnv t)
compile t qs = GlobalEnv <$> compileLoop (compileDefinition t) qs

compileDefinition :: Target t -> Definition -> CompileM (Maybe (KerName, GlobalDecl t))
compileDefinition target defn@Defn{..} = setCurrentRange defName do
  reportSDoc "agda2lambox.compile" 1 $ "Compiling definition: " <+> prettyTCM defName

  -- we skip logical definitions altogether
  ifM (liftTCM $ isLogical $ Arg defArgInfo defType) (pure Nothing) do

  -- prepend kername
  fmap (qnameToKerName defName,) <$> case theDef of
    GeneralizableVar{} -> pure Nothing

    Axiom{} -> do
      typ <- whenTyped target $ compileTopLevelType defType
      pure $ Just $ ConstantDecl $ ConstantBody typ Nothing

    Constructor{conData} -> Nothing <$ requireDef conData

    Function{} -> compileFunction target defn

    d | isDataOrRecDef d -> compileInductive target defn

    Primitive{..} -> do
      reportSDoc "agda2lambox.compile" 5 $
        "Found primitive: " <> prettyTCM defName <> ". Compiling it as axiom."

      typ <- whenTyped target $ compileTopLevelType defType
      pure $ Just $ ConstantDecl $ ConstantBody typ Nothing

    _ -> genericError $ "Cannot compile: " <> prettyShow defName
