{-# LANGUAGE NamedFieldPuns #-}
module Agda2Lambox.Compile 
  ( compileDefinition
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
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Function  ( compileFunction  )
import Agda2Lambox.Compile.Inductive ( compileInductive )
import LambdaBox


compileDefinition :: QName -> CompileM (Maybe (KerName, GlobalDecl))
compileDefinition q = do
  defn@Defn{..} <- liftTCM $ getConstInfo q

  fmap (qnameToKerName defName,) <$> -- prepend kername
    case theDef of
      Constructor{conData} -> Nothing <$ requireDef conData
      Function{}           -> compileFunction defn
      d | isDataOrRecDef d -> compileInductive defn
      _                    -> Nothing <$ (liftIO $ putStrLn $ "Skipping " <> prettyShow defName)
