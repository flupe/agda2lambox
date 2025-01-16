{-# LANGUAGE NamedFieldPuns #-}
module Agda2Lambox.Compile.Utils
  ( modNameToModPath
  , qnameToKName
  , toInductive
  , toConApp
  ) where

import Control.Monad.IO.Class ( liftIO )
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe, listToMaybe )

import Agda.Syntax.Abstract.Name
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Datatypes ( getConstructors, getConstructorData )
import Agda.TypeChecking.Monad.Base ( TCM )
import Agda.Compiler.Backend ( pattern Datatype, pattern Record, recMutual, dataMutual, getConstInfo, theDef )

import LambdaBox qualified as LBox


-- | Convert and Agda module name to its "equivalent" λ□ module path.
modNameToModPath :: ModuleName -> LBox.ModPath
modNameToModPath = LBox.MPFile . map prettyShow . mnameToList


-- | Convert and Agda definition name to a λ□ kernel name.
qnameToKName :: QName -> LBox.KerName
qnameToKName qn =
  LBox.KerName
    (modNameToModPath $ qnameModule qn)
    (prettyShow $ qnameName qn)


dataOrRecMutuals :: QName -> TCM [QName]
dataOrRecMutuals q = do
  defn <- theDef <$> getConstInfo q
  case defn of
    Datatype{dataMutual} -> pure $ fromMaybe [] dataMutual
    Record  {recMutual}  -> pure $ fromMaybe [] recMutual
    _                    -> fail "not a datatype or record"

-- | Fetch the λ□ inductive associated with a @QName@.
toInductive :: QName -> TCM LBox.Inductive
toInductive q = do
  names <- dataOrRecMutuals q
  let repr = fromMaybe q $ listToMaybe names
  let idx  = fromMaybe 0 $ elemIndex q names
  pure $ LBox.Inductive (qnameToKName repr) idx


-- | Compile a constructor application to λ□.
toConApp :: QName -> [LBox.Term] -> TCM LBox.Term
toConApp qn es = do
  dt   <- getConstructorData qn
  ctrs <- getConstructors dt
  ind  <- toInductive dt
  let idx = fromMaybe 0 $ qn `elemIndex` ctrs
  pure $ LBox.LCtor ind idx es
