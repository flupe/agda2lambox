{-# LANGUAGE NamedFieldPuns, DataKinds #-}
-- | Convert Agda functions to λ□ constant declarations
module Agda2Lambox.Compile.Function 
  ( compileFunction
  ) where

import Control.Monad ( forM, when, filterM, unless )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( elemIndex )
import Data.Maybe ( isNothing, fromMaybe )

import Agda.Syntax.Abstract.Name ( QName, qnameModule )
import Agda.TypeChecking.Monad.Base hiding ( None )
import Agda.Compiler.ToTreeless ( toTreeless )
import Agda.Compiler.Backend ( getConstInfo, funInline )
import Agda.Syntax.Treeless ( EvaluationStrategy(EagerEvaluation) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.Syntax.Common ( hasQuantityω )
import Agda.Utils.Monad (guardWithError, whenM)
import Agda.Utils.Lens ( (^.) )

import Agda.Utils ( etaExpandCtor, treeless, pp )
import Agda2Lambox.Compile.Target
import Agda2Lambox.Compile.Utils
import Agda2Lambox.Compile.Monad
import Agda2Lambox.Compile.Term ( compileTerm )

import LambdaBox qualified as LBox
import LambdaBox.Env


-- | Check whether a definition is a function.
isFunction :: Definition -> Bool
isFunction Defn{..} | Function{} <- theDef = True
isFunction _ = False


-- | Convert a function body to a Lambdabox term.
compileFunctionBody :: [QName] -> Definition -> CompileM LBox.Term
compileFunctionBody ms Defn{defName, theDef} = do
  Just t <- liftTCM $ treeless defName
  compileTerm ms =<< liftTCM (etaExpandCtor t)


-- | Whether to compile a function definition to λ□.
shouldCompileFunction :: Definition -> Bool
shouldCompileFunction def@Defn{theDef} | Function{..} <- theDef
  = not (theDef ^. funInline) -- not inlined (from module application)
    && isNothing funExtLam    -- not a pattern-lambda-generated function (inlined by the treeless translation)
    && isNothing funWith      -- not a with-generated function           (inlined by the treeless translation)
    && hasQuantityω def       -- non-erased

-- | Convert a function definition to a λ□ declaration.
compileFunction :: Target t -> Definition -> CompileM (Maybe (LBox.GlobalDecl t))
compileFunction t defn | not (shouldCompileFunction defn) = return Nothing
compileFunction t defn@Defn{theDef} = do
  let Function{funMutual = Just mutuals} = theDef

  defs <- liftTCM $ mapM getConstInfo mutuals

  unless (all isFunction defs) $
    fail "only mutually defined functions are supported."

  -- the mutual functions that we actually compile
  -- (so no with-generated functions, etc...)
  let mdefs  = filter shouldCompileFunction defs
  let mnames = map defName mdefs

  -- if the function is not recursive, just compile the body
  if null mdefs then
    Just . ConstantDecl . ConstantBody (catchall t LBox.TBox) . Just <$> compileFunctionBody [] defn

  -- otherwise, take fixpoint
  else do
    let k = fromMaybe 0 $ elemIndex (defName defn) mnames

    Just . ConstantDecl . ConstantBody (catchall t LBox.TBox) . Just . flip LBox.LFix k <$>
      forM mdefs \def@Defn{defName} -> do
        body <- compileFunctionBody mnames def
        return LBox.Def
          { dName = LBox.Named $ prettyShow defName
          , dBody = body
          , dArgs = 0
          }
