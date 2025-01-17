{-# LANGUAGE NamedFieldPuns #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Compile.Function 
  ( compileFunction
  ) where

import Control.Monad ( forM, when )
import Data.List ( elemIndex )
import Data.Maybe ( isNothing, fromMaybe )

import Agda.Syntax.Abstract.Name ( QName, qnameModule )
import Agda.TypeChecking.Monad.Base
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo, funInline )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils.Monad (guardWithError)
import Agda.Utils.Lens ( (^.) )

import Agda.Utils ( etaExpandCtor )
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Term ( compileTerm )
import LambdaBox qualified as LBox


-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False


-- | Convert a function body to a Lambdabox term.
compileFunctionBody :: [QName] -> Definition -> TCM LBox.Term
compileFunctionBody ms Defn{defName} = do
    Just t <-toTreeless EagerEvaluation defName
    compileTerm ms =<< etaExpandCtor t


-- | Whether to compile a function definition to λ□.
shouldCompileFunction :: Definition -> Bool
shouldCompileFunction def@Defn{theDef} | Function{..} <- theDef
  = not (theDef ^. funInline) -- not inlined (from module application)
    && isNothing funExtLam    -- not a pattern-lambda-generated function   NOTE(flupe): ?
    && isNothing funWith      -- not a with-generated function             NOTE(flupe): ?
    && hasQuantityω def       -- non-erased


-- | Convert a function definition to a λ□ declaration.
compileFunction :: Definition -> TCM (Maybe LBox.GlobalDecl)
compileFunction defn | not (shouldCompileFunction defn) = return Nothing
compileFunction defn@Defn{defName, theDef} = do
    let Function{funMutual = Just ms} = theDef

    -- if the function is at most recursive with itself, just compile body
    if null ms then
      Just. LBox.ConstantDecl . Just <$> compileFunctionBody [] defn

    -- otherwise, take fixpoint
    else do
      mdefs :: [Definition] <- mapM getConstInfo ms

      when (any (not . isFunction) mdefs) $ fail "only mutually defined functions are supported."

      let k = fromMaybe 0 $ elemIndex defName ms

      Just . LBox.ConstantDecl . Just . flip LBox.LFix k <$>
        forM mdefs \def@Defn{defName} -> do
          body <- compileFunctionBody ms def
          return LBox.Def
            { dName = LBox.Named $ prettyShow defName
            , dBody = body
            , dArgs = 0
            }
