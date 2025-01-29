{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE GADTs, BangPatterns #-}
-- | The agda2lambox Agda backend
module Main (main) where

import Control.Monad ( unless, when, forM_, filterM )
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData(rnf) )
import Data.Function ( (&) )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef' )
import Data.Maybe ( fromMaybe, catMaybes )
import Data.Version ( showVersion )
import Data.Text ( pack )
import GHC.Generics ( Generic )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(..) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )
import Data.Kind (Type)
import Data.Text.Lazy.IO qualified as LText

import Paths_agda2lambox ( version )

import Agda.Compiler.Common
import Agda.Compiler.Backend
import Agda.Main ( runAgda )
import Agda.Syntax.Internal ( clauseWhereModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )
import Agda.Syntax.Common.Pretty ( pretty, prettyShow )
import Agda.Utils.Monad ( whenM )

import Agda.Utils ( pp, hasPragma, isDataOrRecDef, filterOutWhere )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile       (compile)
import CoqGen    ( prettyCoq  )
import SExpr     ( prettySexp )
import LambdaBox.Env
import LambdaBox.Names (KerName)


main :: IO ()
main = runAgda [agda2lambox]

data Output = RocqOutput | AstOutput
  deriving (Eq, Show, Generic, NFData)

-- | Backend options.
data Options = forall t. Options
  { optOutDir :: Maybe FilePath
  , optTarget :: Target t
  , optOutput :: Output
  }

instance NFData Options where
  rnf (Options m t o) = rnf m `seq` rnf t `seq` rnf o

-- | Setter for output directory option.
outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts { optOutDir = Just dir }

typedOpt :: Monad m => Options -> m Options
typedOpt opts = return opts { optTarget = ToTyped }

rocqOpt :: Monad m => Options -> m Options
rocqOpt opts = return opts { optOutput = RocqOutput }

-- | Default backend options.
defaultOptions :: Options
defaultOptions = Options
  { optOutDir = Nothing
  , optTarget = ToUntyped
  , optOutput = AstOutput
  }

-- | Backend module environments.
data BackendEnv = forall t. BackendEnv 
  { backendTarget :: Target t
  , backendOutput :: Output
  , backendOutDir :: Maybe FilePath
  , backendEnv    :: IORef [(KerName, GlobalDecl t)]
  }

type ModuleEnv  = ()
type ModuleRes  = ()

-- | The adga2lambox backend.
agda2lambox :: Backend
agda2lambox = Backend backend
  where
    backend :: Backend' Options BackendEnv ModuleEnv ModuleRes QName
    backend = Backend'
      { backendName           = "agda2lambox"
      , backendInteractTop    = Nothing
      , backendInteractHole   = Nothing
      , backendVersion        = Just $ pack $ showVersion version
      , options               = defaultOptions
      , commandLineFlags      =
          [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
            "Write output files to DIR. (default: project root)"
          , Option ['t'] ["typed"] (NoArg typedOpt) 
            "Compile to typed λ□ environments."
          , Option ['c'] ["rocq"] (NoArg rocqOpt) 
            "Output a Rocq file."
          ]
      , isEnabled             = \ _ -> True
      , preCompile            = backendSetup
      , postCompile           = \ _ _ _ -> return ()
      , preModule             = moduleSetup
      , postModule            = writeModule
      , compileDef            = \ _ _ _ -> pure . defName
      , scopeCheckingSuffices = False
      , mayEraseType          = \ _ -> return True
      }


backendSetup :: Options -> TCM BackendEnv
backendSetup Options{..} = do
  env <- liftIO $ newIORef []
  pure BackendEnv 
    { backendOutDir = optOutDir
    , backendTarget = optTarget
    , backendOutput = optOutput
    , backendEnv    = env
    }

moduleSetup
  :: BackendEnv -> IsMain -> TopLevelModuleName -> Maybe FilePath
  -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  pure $ Recompile ()

writeModule
  :: BackendEnv -> ModuleEnv -> IsMain -> TopLevelModuleName
  -> [QName]
  -> TCM ModuleRes
writeModule BackendEnv{..} menv isMain m defs   = do
  defs' <- filterOutWhere defs
  liftIO $ putStrLn $ prettyShow m
  compile backendTarget backendEnv defs'

  when (isMain == IsMain) do
    env      <- liftIO $ readIORef backendEnv
    programs <- filterM hasPragma defs'
    outDir   <- flip fromMaybe backendOutDir <$> compileDir

    liftIO $ createDirectoryIfMissing True outDir

    let fileName = (outDir </>) . moduleNameToFileName m
        coqMod   = CoqModule (GlobalEnv env) (map qnameToKName programs)

    liftIO do
      putStrLn $ "Writing " <> fileName ".txt"
      pp coqMod <> "\n" & writeFile (fileName ".txt")

    liftIO $ case backendOutput of
      RocqOutput -> do
        putStrLn $ "Writing " <> fileName ".v"
        prettyCoq backendTarget coqMod <> "\n"
          & writeFile (fileName ".v")

      AstOutput -> do
        putStrLn $ "Writing " <> fileName ".ast"
        prettySexp backendTarget coqMod <> "\n"
          & LText.writeFile (fileName ".ast")
