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
import Agda.Utils (pp, unqual)

import qualified LambdaBox as L
import Agda2Lambox.Convert ( convert )
import Agda2Lambox.Monad ( runC0 )
import Lambox2Coq

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
  , tterm :: TTerm
  , lterm :: L.Term
  }

instance Show CompiledDef' where
  show CompiledDef{..} =
    unlines [ "-- Treeless"
            , prettyShow (qnameName name) <> " = " <> prettyShow tterm
            , "-- Haskell LambdaBox"
            , prettyShow (qnameName name) <> " = " <> show lterm
            , "-- Coq LambdaBox"
            , term2Coq lterm
            ]

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
        tm <- runC0 defName (convert tterm)
        let tm' = L.Fix [L.Def (L.Named $ pp defName) tm 0] 0
        Function{..} <- return theDef
        Just ds <- return funMutual
        case ds of
          []  -> return $ Just $ CompiledDef defName tterm tm
          [x] -> return $ Just $ CompiledDef defName tterm tm'
          _ -> error "No mutual functons supported yet"

coqModuleTemplate :: [(String, String)] -> String
coqModuleTemplate coqterms = unlines $
  [ "From Coq Require Import Extraction."
  , "From RustExtraction Require Import Loader."
  , "From MetaCoq.Erasure.Typed Require Import ExAst."
  , "From MetaCoq.Common Require Import BasicAst."
  , "From Coq Require Import Arith."
  , "From Coq Require Import Bool."
  , "From Coq Require Import List."
  , "From Coq Require Import Program."
  , ""
  ] ++ map (uncurry coqDefTemplate) coqterms ++
  [ "From RustExtraction Require Import ExtrRustBasic."
  , "From RustExtraction Require Import ExtrRustUncheckedArith."
  ] ++ map (coqExtractTemplate "dummy" . fst) coqterms

coqDefTemplate :: String -> String -> String
coqDefTemplate n d =  "Definition " <> n <> " : term := " <> d <> ".\n"

coqExtractTemplate :: FilePath -> String -> String
coqExtractTemplate fp n = "Redirect \"./" <> n <> ".rs\" Rust Extract " <> n <> "."

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [CompiledDef]
            -> TCM ModuleRes
writeModule opts _ _ m (catMaybes -> cdefs) = do
  outDir <- compileDir
  liftIO $ putStrLn (moduleNameToFileName m "v")
  let outFile = fromMaybe outDir (optOutDir opts) <> "/" <> moduleNameToFileName m ".v"
  unless (null cdefs) $ liftIO
    $ writeFile outFile
    $ coqModuleTemplate (map (\cdef -> ((unqual $ name cdef), term2Coq (lterm cdef))) cdefs)
