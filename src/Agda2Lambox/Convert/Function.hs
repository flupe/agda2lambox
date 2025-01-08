{-# LANGUAGE NamedFieldPuns #-}
module Agda2Lambox.Convert.Function 
  ( convertFunction
  ) where

import Control.Monad.Reader ( ask, liftIO )
import Control.Monad ( forM )
import Data.List ( elemIndex )

import Utils

import Agda ( liftTCM )
import Agda.Lib ()
import Agda.Utils
import Agda.Syntax.Abstract.Name ( qnameModule )
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Env ( withCurrentModule )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )

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
    convert t

-- | Convert a function to a Lambdabox term.
convertFunction :: Definition :~> Term
convertFunction defn@Defn{defName, theDef} =
  withCurrentModule (qnameModule defName) do
    let Function{funMutual = Just ms} = theDef

    if null ms then convertFunctionBody defn
    else do
      mdefs :: [Definition] <- mapM getConstInfo ms

      if (any (not . isFunction) mdefs) then
        fail "Induction-recursion and other weird mutual defs not supported."
      else inMutuals ms do
        -- NOTE(flupe): 
        --   maybe reverse ms here?
        --   it's unclear in which order mutual fixpoints are added to the local context
        let Just k = elemIndex defName ms

        flip LFix k <$> forM mdefs \def@Defn{defName} -> do
          body <- convertFunctionBody def
          return Def
            { dName = Named $ prettyShow defName
            , dBody = body
            , dArgs = 0 -- TODO(flupe): when is this ever not zero?
            }
