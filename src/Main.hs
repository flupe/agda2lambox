{-# LANGUAGE DeriveGeneric, DeriveAnyClass, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

import Agda.Utils ( pp, hasPragma, isDataOrRecDef )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile       (compile)
import CoqGen    ( prettyCoq  )
import SExpr     ( prettySexp )
import LambdaBox.Env


main :: IO ()
main = runAgda [agda2lambox]

-- | Backend options.
data Options = forall t. Options
  { optOutDir :: Maybe FilePath
  , optTarget :: Target t
  }

instance NFData Options where rnf (Options m t) = rnf m `seq` rnf t

-- | Setter for output directory option.
outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts { optOutDir = Just dir }

typedOpt :: Monad m => Options -> m Options
typedOpt opts = return opts { optTarget = ToTyped }

-- | Default backend options.
defaultOptions :: Options
defaultOptions = Options
  { optOutDir = Nothing
  , optTarget = ToUntyped
  }

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
      , backendInteractTop    = Nothing
      , backendInteractHole   = Nothing
      , backendVersion        = Just $ pack $ showVersion version
      , options               = defaultOptions
      , commandLineFlags      =
          [ Option ['o'] ["out-dir"] (ReqArg outdirOpt "DIR")
            "Write output files to DIR. (default: project root)"
          , Option ['t'] ["typed"] (NoArg typedOpt) 
            "Compile to typed λ□ environments."
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



-- | Remove all the names defined in where clauses from the list of names.
-- Assumes that the order is the one given by Agda, that is:
-- definitions in (possibly anonymous) modules coming from where clauses appear 
-- right after their parent function definition.
filterOutWhere :: [QName] -> TCM [QName]
filterOutWhere [] = pure []
filterOutWhere (q:qs) = do
  ws <- getWheres q
  let qs' = dropWhile (isChild ws) qs
  (q:) <$> filterOutWhere qs'

  where
  isChild :: [ModuleName] -> QName -> Bool
  isChild ws r = any (r `isChildOfMod`) ws

  isChildOfMod :: QName -> ModuleName -> Bool
  isChildOfMod q mod = qnameModule q `isLeChildModuleOf` mod

  getWheres :: QName -> TCM [ModuleName]
  getWheres q = do
    def <- getConstInfo q
    pure case theDef def of
      Function{..} -> catMaybes $ map clauseWhereModule funClauses
      _            -> []

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
writeModule Options{..} menv IsMain m defs = do
  programs <- filterM hasPragma defs
  outDir   <- flip fromMaybe optOutDir <$> compileDir

  defs' <- filterOutWhere defs

  env <- compile optTarget $ reverse defs'

  liftIO $ createDirectoryIfMissing True outDir

  let fileName = (outDir </>) . moduleNameToFileName m
      coqMod   = CoqModule env (map qnameToKName programs)

  liftIO do
    putStrLn $ "Writing " <> fileName ".{v,txt,ast}"

    pp coqMod <> "\n"
      & writeFile (fileName ".txt")

    prettyCoq optTarget coqMod <> "\n"
      & writeFile (fileName ".v")

    prettySexp optTarget coqMod <> "\n"
      & LText.writeFile (fileName ".ast")
