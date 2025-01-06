{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )
import Data.Maybe ( fromMaybe, catMaybes, isJust )
import Data.Version ( showVersion )
import GHC.Generics ( Generic )
import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>) )

import Paths_agda2lambox ( version )

import Agda.Lib
import Agda.Utils (pp, unqual)

import qualified LambdaBox as L
import Agda2Lambox.Convert ( convert )
import Agda2Lambox.Monad ( runC0, inMutuals )
import Lambox2Coq


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

data CompiledDef = CompiledDef
  { name  :: QName
  , tterm :: TTerm
  , atype :: Type 
  , ltype :: L.Type 
  , lterm :: L.Term
  }

instance Show CompiledDef where
  show CompiledDef{..} =
    let pre  = pp (qnameName name) <> " = " in
    let preT = pp (qnameName name) <> " : " in  
    unlines [ "=== SOURCE ==="
            , preT <> pp atype  
            , pre  <> pp tterm
            , "=== COMPILED ==="
            , preT <> pp ltype
            , pre  <> pp lterm
            -- , pre <> term2Coq lterm
            ]

backend :: Backend' Options Options ModuleEnv ModuleRes (Maybe CompiledDef)
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

-- | Which Agda definitions to ignore.
ignoreDef :: Definition -> Bool
ignoreDef d@Defn{..}
  | hasQuantity0 d
  = True
  | Function {..} <- theDef
  = (theDef ^. funInline) -- inlined functions (from module application)
  -- || funErasure           -- @0 functions
  || isJust funExtLam     -- pattern-lambdas
  || isJust funWith       -- with-generated
  | otherwise
  = True

compile :: Options -> ModuleEnv -> IsMain -> Definition -> TCM (Maybe CompiledDef)
compile _ _ _ defn | ignoreDef defn = return Nothing
compile opts tlm _ defn@Defn{..} =
  withCurrentModule (qnameModule defName) do
    Just tterm <- toTreeless EagerEvaluation defName
    let Function{..} = theDef
        Just ds      = funMutual
    tm <- runC0 (inMutuals ds $ convert tterm)
    ty <- runC0 (convert (unEl $ defType)) 
    let tm' = case ds of []    -> tm
                         [d]   -> L.Fix [L.Def (L.Named $ pp defName) tm 0] 0
                         _:_:_ -> error "Mutual recursion not supported."
    return $ Just $ CompiledDef defName tterm defType ty tm'

writeModule :: Options -> ModuleEnv -> IsMain -> TopLevelModuleName
            -> [Maybe CompiledDef]
            -> TCM ModuleRes
writeModule opts _ _ m (catMaybes -> cdefs) = do
  compDir <- compileDir

  let outDir   = fromMaybe compDir (optOutDir opts)
      fileName = (outDir </>) . moduleNameToFileName m

  liftIO $ createDirectoryIfMissing True outDir

  unless (null cdefs) $ liftIO do
    putStrLn $ "Writing " <> fileName ".v"

    writeFile (fileName ".txt")
      $ unlines (show <$> cdefs)

    writeFile (fileName ".v")
      $ coqModuleTemplate
      $ map (\cdef -> (unqual (name cdef), term2Coq (lterm cdef))) cdefs

  where
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
