module LambdaBox where

import Data.Maybe ( fromMaybe )
import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.DeepSeq ( NFData(..) )

import System.Console.GetOpt ( OptDescr(Option), ArgDescr(ReqArg) )

import Data.Version ( showVersion )
import Paths_agda2lambox ( version )

import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( qnameName, qnameModule )
import Agda.Syntax.TopLevelModuleName ( TopLevelModuleName, moduleNameToFileName )

import Agda.Compiler.Common ( curIF, compileDir )
import Agda.Compiler.Backend ( Backend(..), Backend'(..), Recompile(..), IsMain )

import Agda.TypeChecking.Monad.Base ( Definition(..) )
import Agda.TypeChecking.Monad
  ( TCM, withCurrentModule, iInsideScope, setScope
  , CompilerPragma(..), getUniqueCompilerPragma )

type Ident = String
type KName = String
type Inductive = String

data Name = Anon | Named Ident
  deriving (Eq, Show)

data Def = Def Name Term Int
  deriving (Eq, Show)

data Term
  = Box
  | Var Int
  | Lam Name Term
  | App Term Term
  | Const KName
  | Ctor Inductive Int
  | Case Inductive Int Term [(Int, Term)]
  | Fix [Def] Int
  deriving (Eq, Show)
