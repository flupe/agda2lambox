{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NamedFieldPuns #-}
-- | The agda2lambox Agda backend
module Main (main) where

import Control.Monad ( unless, when, forM_, filterM )
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData )
import Data.Function ( (&) )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef' )
import Data.Maybe ( fromMaybe, catMaybes )
import Data.Version ( showVersion )
import GHC.Generics ( Generic )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )

import Paths_agda2lambox ( version )

import Agda.Compiler.Common
import Agda.Compiler.Backend
import Agda.Main ( runAgda )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.Syntax.Common.Pretty ( pretty, prettyShow )
import Agda.Utils.Monad ( whenM )

import Agda.Utils ( pp, hasPragma, isDataOrRecDef )
import Agda2Lambox.Compile.Utils (qnameToKName)
import Agda2Lambox.Compile.Monad (compileLoop)
import Agda2Lambox.Compile       (compileDefinition)
import CoqGen    ( ToCoq(ToCoq) )
import LambdaBox ( KerName, GlobalDecl, qnameToKerName, CoqModule(..) )


main :: IO ()
main = runAgda [agda2lambox]

-- | Backend options.
data Options = Options { optOutDir :: Maybe FilePath }
  deriving (Generic, NFData)

-- | Setter for output directory option.
outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts { optOutDir = Just dir }

-- | Default backend options.
defaultOptions :: Options
defaultOptions = Options { optOutDir = Nothing }

-- | Backend module environments.
type ModuleEnv = ()
type ModuleRes = ()

-- | The adga2lambox backend.
agda2lambox :: Backend
agda2lambox = Backend backend
  where
    backend :: Backend' Options Options ModuleEnv ModuleRes QName
    backend = Backend'
      { backendName           = "agda2lambox"
      , backendVersion        = Just $ showVersion version
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
      , compileDef            = \ _ _ _ -> pure . defName
      , scopeCheckingSuffices = False
      , mayEraseType          = \ _ -> return True
      }


moduleSetup
  :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
  -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  pure $ Recompile ()


writeModule
  :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
  -> [QName]
  -> TCM ModuleRes
writeModule opts menv NotMain _ _   = pure ()
writeModule opts menv IsMain m defs = do
  programs <- filterM hasPragma defs
  outDir   <- flip fromMaybe (optOutDir opts) <$> compileDir
  decls    <- compileLoop compileDefinition $ reverse defs

  liftIO $ createDirectoryIfMissing True outDir

  let fileName = (outDir </>) . moduleNameToFileName m
      coqMod   = CoqModule decls (map qnameToKName programs)

  unless (null decls) $ liftIO do
    putStrLn $ "Writing " <> fileName ".{v,txt}"

    pp coqMod <> "\n"
      & writeFile (fileName ".txt")

    pp (ToCoq coqMod) <> "\n"
      & writeFile (fileName ".v")
