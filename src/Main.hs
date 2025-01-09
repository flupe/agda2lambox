{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Main where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )
import Data.Maybe ( fromMaybe, catMaybes, isJust, isNothing )
import Data.Version ( showVersion )
import GHC.Generics ( Generic )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )

import Paths_agda2lambox ( version )

import Agda.Lib hiding ( (<?>), pretty )
import Agda.Syntax.Common.Pretty ( (<?>), pretty )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils ( pp, unqual )

import Agda2Lambox.Convert ( convert )
import Agda2Lambox.Convert.Function ( convertFunction )
import Agda2Lambox.Convert.Data     ( convertDatatype )
import Agda2Lambox.Monad ( runC0, inMutuals )
import CoqGen ( ToCoq(ToCoq) )
import LambdaBox ( KerName, GlobalDecl, qnameToKerName )


main :: IO ()
main = runAgda [Backend backend]

-- | LambdaBox backend options.
data Options = Options { optOutDir :: Maybe FilePath }
  deriving (Generic, NFData)

-- | Setter for backend output directory option.
outdirOpt :: Monad m => FilePath -> Options -> m Options
outdirOpt dir opts = return opts { optOutDir = Just dir }

defaultOptions :: Options
defaultOptions = Options { optOutDir = Nothing }


type ModuleEnv = ()
type ModuleRes = ()

backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe (KerName, GlobalDecl))
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


compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM (Maybe (KerName, GlobalDecl))
compile opts tlm _ def@Defn{..} =
  fmap (qnameToKerName defName,) <$> -- prepend kername
    case theDef of

      -- TODO(flupe): offload the check to Convert.Function
      Function{..}
        | not (theDef ^. funInline)  -- not inlined (from module application)
        , isNothing funExtLam        -- not a pattern-lambda-generated function   NOTE(flupe): ?
        , isNothing funWith          -- not a with-generated function             NOTE(flupe): ?
        , hasQuantityω def           -- non-erased
        -> Just <$> runC0 (convertFunction def)

      Datatype{} -> Just <$> runC0 (convertDatatype def)

      _          -> Nothing <$ (liftIO $ putStrLn $ "Skipping " <> prettyShow defName)


writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [Maybe (KerName, GlobalDecl)]
            -> TCM ModuleRes
writeModule opts _ _ m (reverse . catMaybes -> cdefs) = do
  compDir <- compileDir

  let outDir   = fromMaybe compDir (optOutDir opts)
      fileName = (outDir </>) . moduleNameToFileName m

  liftIO $ createDirectoryIfMissing True outDir

  unless (null cdefs) $ liftIO do
    putStrLn $ "Writing " <> fileName ".v"

    writeFile (fileName ".txt")
      $ unlines (show <$> cdefs)

    writeFile (fileName ".v")
      $ (<> "\n")
      $ coqModuleTemplate cdefs

  where
  coqModuleTemplate :: [(KerName, GlobalDecl)] -> String
  coqModuleTemplate coqterms = unlines
    [ "From MetaCoq.Common Require Import BasicAst Kernames Universes."
    , "From MetaCoq.Erasure Require Import EAst."
    , "From MetaCoq.Utils Require Import bytestring MCString."
    , "From Coq Require Import List."

    , ""
    , "Import ListNotations."
    , "Open Scope pair_scope."
    , "Open Scope bs."
    , ""
    ] ++ coqDecls coqterms

  coqDecls :: [(KerName, GlobalDecl)] -> String
  coqDecls ds = pp $ "Definition env : global_declarations := " <?> (pretty (ToCoq ds) <> ".")
