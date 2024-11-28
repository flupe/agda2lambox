{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Maybe ( fromMaybe, catMaybes )
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda2lambox ( version )

import Agda.Lib

import qualified LambdaBox as L
import Agda2Lambox.Convert ( convert )
import Agda2Lambox.Monad ( runC0 )

main = runAgda [Backend backend]

data Options = Options { optOutDir :: Maybe FilePath }

instance NFData Options where
  rnf _ = ()

outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts{ optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options{ optOutDir = Nothing }

type ModuleEnv   = ()
type ModuleRes   = ()
data CompiledDef' = CompiledDef
  { name :: QName
  , term :: L.Term
  }

instance Show CompiledDef' where
  show CompiledDef{..} = prettyShow (qnameName name) <> " = " <> show term

type CompiledDef = Maybe CompiledDef'


backend :: Backend' Options Options ModuleEnv ModuleRes CompiledDef
backend = Backend'
  { backendName           = "agda2lambox"
  , backendVersion        = Just (showVersion version)
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
        "Write output files to DIR. (default: project root)"
      ]
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = moduleSetup
  , postModule            = writeModule
  , compileDef            = compile
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

moduleSetup :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
            -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  return $ Recompile ()

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM CompiledDef
compile opts tlm _ Defn{..}
  = withCurrentModule (qnameModule defName)
  $ getUniqueCompilerPragma "AGDA2LAMBOX" defName >>= \case
      Nothing -> return Nothing
      Just (CompilerPragma _ _) -> do
        Just tterm <- toTreeless EagerEvaluation defName
        Just . (CompiledDef defName) . fst <$> runC0 (convert tterm)

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [CompiledDef]
            -> TCM ModuleRes
writeModule opts _ _ m (catMaybes -> cdefs) = do
  outDir <- compileDir
  liftIO $ putStrLn (moduleNameToFileName m "txt")
  let outFile = fromMaybe outDir (optOutDir opts) <> "/" <> moduleNameToFileName m "txt"
  unless (null cdefs) $ liftIO
    $ writeFile outFile
    $ "*** module " <> prettyShow m <> " ***\n" <> unlines (show <$> cdefs)
