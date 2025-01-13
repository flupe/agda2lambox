{-# LANGUAGE NamedFieldPuns #-}
-- | Convert Agda datatypes to λ□ inductive declarations
module Agda2Lambox.Convert.Data
  ( convertDatatype
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM )
import Data.List ( elemIndex )

import Utils

import Agda ( liftTCM )
import Agda.Lib ()
import Agda.Utils
import Agda.Syntax.Abstract.Name ( qnameModule, qnameName )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.TypeChecking.Datatypes ( ConstructorInfo(..), getConstructorInfo )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )

import LambdaBox

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
import Agda.Utils.Monad (guardWithError)


-- | Convert a datatype definition to a Lambdabox declaration.
convertDatatype :: Definition :~> GlobalDecl
convertDatatype defn@Defn{defName, theDef} =
  withCurrentModule (qnameModule defName) do
    let Datatype{..} = theDef

    ctors :: [ConstructorBody]
      <- forM dataCons \cname -> do
           DataCon arity <- getConstructorInfo cname
           return Ctor
             { ctorName = prettyShow $ qnameName cname
             , ctorArgs = arity
             }

    let
      inductive = OneInductive
        { indName          = prettyShow $ qnameName defName
        , indPropositional = False
            -- TODO(flupe): ^ take care of this (use datatypeSort to figure this out)
        , indKElim         = IntoAny
            -- TODO(flupe): also take care of this (with the Sort)
        , indCtors         = ctors
        , indProjs         = []
        }

    return $ InductiveDecl $ MutualInductive
      { indFinite = Finite
          -- NOTE(flupe): Agda's datatypes are *always* finite?
          -- Co-induction is restricted to records.
          -- We may want to set BiFinite for non-recursive datatypes, but I don't know yet.
      , indPars   = 0
      , indBodies = [inductive]
      }
