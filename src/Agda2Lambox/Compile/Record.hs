{-# LANGUAGE NamedFieldPuns #-}
-- | Convert Agda records to λ□ inductive declarations
module Agda2Lambox.Compile.Record
  ( compileRecord
  ) where

import Control.Monad ( forM )
import Control.Monad.Reader ( ask, liftIO )
import Data.List ( elemIndex )
import Data.Maybe ( maybe )

import Agda.Syntax.Abstract.Name ( qnameModule, qnameName )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Datatypes ( ConstructorInfo(..), getConstructorInfo )
import Agda.Syntax.Internal ( ConHead(..), unDom )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( Induction(..) )

import Agda2Lambox.Compile.Utils
import LambdaBox qualified as LBox

inductionToRecKind :: Induction -> LBox.RecursivityKind
inductionToRecKind = \case
  Inductive   -> LBox.Finite
  CoInductive -> LBox.CoFinite

-- | Convert a datatype definition to a Lambdabox declaration.
compileRecord :: Definition -> TCM LBox.GlobalDecl
compileRecord defn@Defn{defName, theDef} =
  let

    Record{..} = theDef
    ConHead{conName, conFields} = recConHead

    fields :: [LBox.ProjectionBody]
      = LBox.Proj . prettyShow . qnameName . unDom <$> recFields

    record = LBox.OneInductive
      { indName          = prettyShow $ qnameName defName
      , indPropositional = False        -- TODO(flupe): take care of this
      , indKElim         = LBox.IntoAny -- TODO(flupe): also take care of this
      , indCtors         = [ LBox.Ctor (prettyShow $ qnameName conName) 
                                  (length conFields) ]
      , indProjs         = fields
      }

  in return $ LBox.InductiveDecl LBox.MutualInductive
    { indFinite = maybe LBox.BiFinite inductionToRecKind recInduction
    , indPars   = 0
        -- TODO(flupe):
        --   ^ double-check, but it doesn't look like 
        --     this could be anything other than 0.
    , indBodies = [record]
    }
