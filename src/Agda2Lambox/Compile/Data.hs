{-# LANGUAGE NamedFieldPuns, ImportQualifiedPost #-}
-- | Convert Agda datatypes to λ□ inductive declarations
module Agda2Lambox.Compile.Data
  ( compileData
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM, when, unless, (>=>) )
import Data.Traversable ( mapM )
import Data.Foldable ( toList )
import Data.List ( elemIndex )
import Data.Maybe ( isJust, listToMaybe )

import Agda.Syntax.Abstract.Name ( qnameModule, qnameName )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.TypeChecking.Datatypes ( ConstructorInfo(..), getConstructorInfo, isDatatype )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo, lookupMutualBlock )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )

import Agda2Lambox.Compile.Utils
import LambdaBox qualified as LBox


-- | Toplevel conversion from a datatype definition to a Lambdabox declaration.
compileData :: Definition -> TCM (Maybe LBox.GlobalDecl)
compileData defn@Defn{defName, defMutual} = do
  let Datatype{..} = theDef defn
  let names = case dataMutual of
        Just [] -> [defName]
        Just xs -> xs
        Nothing -> fail "error"

  -- we consider that the *lowest name* in the mutual block
  -- is the *representative* of the mutual block
  -- i.e that's when we trigger the compilation of the mutual block

  if Just defName /= listToMaybe names then return Nothing
  else do

    -- when it's time to compile the mutual block
    -- we make sure that all definitions in the block are datatypes (for now)

    onlyDatas :: Bool <- and <$> mapM (liftTCM . isDatatype) names

    unless onlyDatas $ fail "not supported: mutual datatypes with non-datatypes"

    bodies <- forM names $ getConstInfo >=> actuallyConvertDatatype

    return $ Just $ LBox.InductiveDecl $ LBox.MutualInductive
      { indFinite = LBox.Finite
          -- NOTE(flupe): Agda's datatypes are *always* finite?
          -- Co-induction is restricted to records.
          -- We may want to set BiFinite for non-recursive datatypes, but I don't know yet.
          -- in anycase, once we also accept coinductive records in the mix, probably we should pick CoFinite
      , indPars   = 0
      , indBodies = bodies
      }



actuallyConvertDatatype :: Definition -> TCM LBox.OneInductiveBody
actuallyConvertDatatype defn@Defn{defName, theDef, defMutual} = do
    let Datatype{..} = theDef

    ctors :: [LBox.ConstructorBody]
      <- forM dataCons \cname -> do
           DataCon arity <- getConstructorInfo cname
           return LBox.Ctor
             { ctorName = prettyShow $ qnameName cname
             , ctorArgs = arity
             }

    return LBox.OneInductive
      { indName          = prettyShow $ qnameName defName
      , indPropositional = False
          -- TODO(flupe): ^ take care of this (use datatypeSort to figure this out)
      , indKElim         = LBox.IntoAny
          -- TODO(flupe): also take care of this (with the Sort)
      , indCtors         = ctors
      , indProjs         = []
      }


