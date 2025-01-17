{-# LANGUAGE NamedFieldPuns, ImportQualifiedPost #-}
-- | Convert Agda datatypes to λ□ inductive declarations
module Agda2Lambox.Compile.Inductive
  ( compileInductive
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM, when, unless, (>=>) )
import Data.Foldable ( toList )
import Data.List ( elemIndex )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.List.NonEmpty qualified as NEL
import Data.Maybe ( isJust, listToMaybe, fromMaybe )
import Data.Traversable ( mapM )

import Agda.Syntax.Abstract.Name ( qnameModule, qnameName )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.TypeChecking.Datatypes ( ConstructorInfo(..), getConstructorInfo, isDatatype )
import Agda.Compiler.Backend ( getConstInfo, lookupMutualBlock )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Internal ( ConHead(..), unDom )
import Agda.Utils.Monad ( unlessM )

import Agda.Utils ( isDataOrRecDef )
import Agda2Lambox.Compile.Utils
import LambdaBox qualified as LBox


-- | Toplevel conversion from a datatype/record definition to a Lambdabox declaration.
compileInductive :: Definition -> TCM (Maybe LBox.GlobalDecl)
compileInductive defn@Defn{defName} = do
  mutuals <- dataOrRecDefMutuals defn

  {- NOTE(flupe):
     if mutuals is []:
       the record/datatype isn't recursive
     if mutuals is [q]:
       then q == defName,
       the record/datatype is inductive/coinductive but not mutually-defined
     otherwise,
       the record/datatype is mutually defined with other things -}

  let items = fromMaybe (NEL.singleton defName) $ NEL.nonEmpty mutuals

  {- also note that we assume the list of mutuals will be the same
     for every record/datatype in the list (especially the order),
     as we make the first item in the list responsible for compiling all of them. -}

  if defName /= NEL.head items then do
    liftIO $ putStrLn $ "Skipping " <> prettyShow defName
    pure Nothing

  else do
    defs <- mapM getConstInfo items

    unless (all (isDataOrRecDef . theDef) defs) $
      fail "mutually-defined datatypes/records *and* functions not supported."

    bodies <- forM defs actuallyConvertInductive

    return $ Just $ LBox.InductiveDecl $ LBox.MutualInductive
      { indFinite = LBox.Finite -- TODO(flupe)
      , indPars   = 0
      , indBodies = NEL.toList bodies
      }

actuallyConvertInductive :: Definition -> TCM LBox.OneInductiveBody
actuallyConvertInductive Defn{defName, theDef, defMutual} = case theDef of
  Datatype{..} -> do

    ctors :: [LBox.ConstructorBody]
      <- forM dataCons \cname -> do
           DataCon arity <- getConstructorInfo cname
           return LBox.Ctor
             { ctorName = prettyShow $ qnameName cname
             , ctorArgs = arity
             }

    pure LBox.OneInductive
      { indName          = prettyShow $ qnameName defName
      , indPropositional = False        -- TODO(flupe)
      , indKElim         = LBox.IntoAny -- TODO(flupe)
      , indCtors         = ctors
      , indProjs         = []
      }

  Record{..} -> do

    let ConHead{conName, conFields} = recConHead
        fields :: [LBox.ProjectionBody] = LBox.Proj . prettyShow . qnameName . unDom <$> recFields

    pure LBox.OneInductive
      { indName          = prettyShow $ qnameName defName
      , indPropositional = False        -- TODO(flupe)
      , indKElim         = LBox.IntoAny -- TODO(flupe)
      , indCtors         = [ LBox.Ctor (prettyShow $ qnameName conName) (length conFields) ]
      , indProjs         = fields
      }

  -- { indFinite = maybe LBox.BiFinite inductionToRecKind recInduction
