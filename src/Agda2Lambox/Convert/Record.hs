{-# LANGUAGE NamedFieldPuns #-}
-- | Convert Agda records to λ□ inductive declarations
module Agda2Lambox.Convert.Record
  ( convertRecord
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM )
import Data.List ( elemIndex )
import Data.Maybe ( maybe )

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
import Agda.Syntax.Internal ( ConHead(..), unDom )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common as A ( Induction(..) )

import LambdaBox

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
import Agda.Utils.Monad (guardWithError)

inductionToRecKind :: Induction -> RecursivityKind
inductionToRecKind = \case
  A.Inductive   -> Finite
  A.CoInductive -> CoFinite

-- | Convert a datatype definition to a Lambdabox declaration.
convertRecord :: Definition :~> GlobalDecl
convertRecord defn@Defn{defName, theDef} =
  withCurrentModule (qnameModule defName) $
    let

      Record{..} = theDef
      ConHead{conName, conFields} = recConHead

      fields :: [ProjectionBody]
        = Proj . prettyShow . qnameName . unDom <$> recFields

      record = OneInductive
        { indName          = prettyShow $ qnameName defName
        , indPropositional = False   -- TODO(flupe): take care of this
        , indKElim         = IntoAny -- TODO(flupe): also take care of this
        , indCtors         = [ Ctor (prettyShow $ qnameName conName) 
                                    (length conFields) ]
        , indProjs         = fields
        }

    in return $ InductiveDecl MutualInductive
      { indFinite = maybe BiFinite inductionToRecKind recInduction
      , indPars   = 0
          -- TODO(flupe):
          --   ^ double-check, but it doesn't look like 
          --     this could be anything other than 0.
      , indBodies = [record]
      }
