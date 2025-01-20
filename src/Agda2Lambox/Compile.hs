{-# LANGUAGE NamedFieldPuns, DataKinds #-}
module Agda2Lambox.Compile 
  ( compile
  ) where

import Control.Monad.IO.Class ( liftIO )
import Data.IORef

import Agda.Compiler.Backend
import Agda.Syntax.Internal ( QName )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Monad ( liftTCM, getConstInfo )
import Agda.Utils.Monad ( whenM )

import Agda.Utils ( hasPragma, isDataOrRecDef )

import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Function  ( compileFunction  )
import Agda2Lambox.Compile.Inductive ( compileInductive )

import LambdaBox.Names
import LambdaBox.Env (GlobalEnv(..), GlobalDecl)

-- | Compile the given names to a λ□ environment.
compile :: Target t -> [QName] -> TCM (GlobalEnv t)
compile t qs = GlobalEnv <$> compileLoop (compileDefinition t) qs

compileDefinition :: Target t -> Definition -> CompileM (Maybe (KerName, GlobalDecl t))
compileDefinition t defn@Defn{..} = do
  fmap (qnameToKerName defName,) <$> -- prepend kername
    case theDef of
      Constructor{conData} -> Nothing <$ requireDef conData
      Function{}           -> compileFunction t defn
      d | isDataOrRecDef d -> compileInductive t defn
      _                    -> Nothing <$ (liftIO $ putStrLn $ "Skipping " <> prettyShow defName)
