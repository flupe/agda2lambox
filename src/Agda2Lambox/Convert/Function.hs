{-# LANGUAGE NamedFieldPuns #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Convert.Function 
  ( convertFunction
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM )
import Data.List ( elemIndex )
import Data.Maybe ( isNothing, isJust )

import Utils

import Agda ( liftTCM )
import Agda.Lib ( (^.), funInline )
import Agda.Utils
import Agda.Syntax.Abstract.Name ( qnameModule )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( hasQuantityω )

import LambdaBox

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
import Agda2Lambox.Convert.Term
import Agda.Utils.Monad (guardWithError)

-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False

-- | Convert a function body to a Lambdabox term.
convertFunctionBody :: Definition :~> Term
convertFunctionBody Defn{defName} =
  withCurrentModule (qnameModule defName) do
    Just t <- liftTCM $ toTreeless EagerEvaluation defName
    convert =<< liftTCM (etaExpandCtor t)

-- | Whether to compile a function definition to λ□.
shouldCompileFunction :: Definition -> Bool
shouldCompileFunction def@Defn{theDef} | Function{..} <- theDef
  = not (theDef ^. funInline)           -- not inlined (from module application)
    && isNothing funExtLam              -- not a pattern-lambda-generated function   NOTE(flupe): ?
    && isNothing funWith                -- not a with-generated function             NOTE(flupe): ?
    && hasQuantityω def                 -- non-erased

-- | Convert a function definition to a λ□ declaration.
convertFunction :: Definition :~> Maybe GlobalDecl
convertFunction defn | not (shouldCompileFunction defn) = return Nothing
convertFunction defn@Defn{defName, theDef} =
  withCurrentModule (qnameModule defName) do
    let Function{funMutual = Just ms} = theDef

    if null ms then 
      Just. ConstantDecl . Just <$> convertFunctionBody defn
    else do
      mdefs :: [Definition] <- mapM getConstInfo ms

      if any (not . isFunction) mdefs then
        fail "Induction-recursion and other weird mutual defs not supported."
      else inMutuals ms do
        let Just k = elemIndex defName ms

        Just . ConstantDecl . Just . flip LFix k <$>
          forM mdefs \def@Defn{defName} -> do
            body <- convertFunctionBody def
            return Def
              { dName = Named $ prettyShow defName
              , dBody = body
              , dArgs = 0 -- TODO(flupe): when is this ever not zero?
              }
