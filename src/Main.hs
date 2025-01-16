{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | The agda2lambox Agda backend
module Main where

import Control.Monad ( unless )
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

import Agda.Lib
import Agda.Utils ( pp, hasPragma )

import Agda2Lambox.Convert ( convert )
import Agda2Lambox.Convert.Function ( convertFunction )
import Agda2Lambox.Convert.Data     ( convertDatatype )
import Agda2Lambox.Convert.Record   ( convertRecord   )
import Agda2Lambox.Monad ( runC0, inMutuals )
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
data ModuleEnv = ModuleEnv 
  { modProgs :: IORef [KerName]
     -- ^ Names of programs to extract in a module
  }

type ModuleRes = ()

-- | The adga2lambox backend.
agda2lambox :: Backend
agda2lambox = Backend backend
  where
    backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe (KerName, GlobalDecl))
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
      , compileDef            = compileDefinition
      , scopeCheckingSuffices = False
      , mayEraseType          = \ _ -> return True
      }


moduleSetup
  :: Options -> IsMain -> TopLevelModuleName -> Maybe FilePath
  -> TCM (Recompile ModuleEnv ModuleRes)
moduleSetup _ _ m _ = do
  setScope . iInsideScope =<< curIF
  Recompile . ModuleEnv <$> liftIO (newIORef [])


compileDefinition
  :: Options -> ModuleEnv -> IsMain
  -> Definition
  -> TCM (Maybe (KerName, GlobalDecl))
compileDefinition opts menv _ def@Defn{..} =
  fmap (qnameToKerName defName,) <$> -- prepend kername
    case theDef of

      Function{} -> do
          -- if the function is annotated with a COMPILE pragma
          -- then it is added to the list of programs to run
          whenM (hasPragma defName) $ 
            liftIO $ modifyIORef' (modProgs menv) (qnameToKerName defName:)

          runC0 (convertFunction def)

      Datatype{} -> runC0 (convertDatatype def)

      Record{}   -> Just <$> runC0 (convertRecord def)

      _          -> Nothing <$ (liftIO $ putStrLn $ "Skipping " <> prettyShow defName)


-- | Collect global definitions and programs in a module
-- and write them to disk.
writeModule
  :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
  -> [Maybe (KerName, GlobalDecl)]
  -> TCM ModuleRes
writeModule opts menv _ m (reverse . catMaybes -> cdefs) = do
  programs   <- liftIO $ readIORef $ modProgs menv
  outDir     <- flip fromMaybe (optOutDir opts) <$> compileDir

  liftIO $ createDirectoryIfMissing True outDir

  let fileName = (outDir </>) . moduleNameToFileName m
      coqMod   = CoqModule cdefs programs

  unless (null cdefs) $ liftIO do
    putStrLn $ "Writing " <> fileName ".{v,txt}"

    pp coqMod <> "\n"
      & writeFile (fileName ".txt")

    pp (ToCoq coqMod) <> "\n"
      & writeFile (fileName ".v")
