{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Convert.Term () where

import Control.Monad.Reader ( ask, liftIO )
import Data.List ( elemIndex )

import Utils

import Agda ( liftTCM, getConstructorData, getConstructors )
import qualified Agda as A
import Agda.Lib ()
import Agda.Utils
import Agda.Syntax.Literal
import Agda.Syntax.Abstract.Name ( QName(..), ModuleName(..) )
import Agda.Syntax.Common.Pretty ( prettyShow )
import Agda.TypeChecking.Primitive.Base ( getBuiltinName )
import Agda.Compiler.Backend ( builtinNat, builtinZero, builtinSuc )

import LambdaBox
import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class

-- quick fix, remove this asap
app :: L.Term -> L.Term -> L.Term
app (LCtor ind k es) arg = LCtor ind k (es ++ [arg])
app t arg                = LApp t arg

-- | Compiling (treeless) Agda terms into Lambox expressions.
instance A.TTerm ~> L.Term where
  go = \case
    A.TVar n   -> return $ LRel n
    A.TPrim pr -> go pr 
    A.TDef qn -> do
      Env{..} <- ask
      return case qn `elemIndex` mutuals of
        Nothing -> LConst $ qnameToKerName qn
        Just i  -> LRel   $ i + boundVars
    A.TApp t args -> do
      ct    <- go t
      cargs <- mapM go args
      return $ foldl app ct cargs
    A.TLam t -> inBoundVar $ LLam <$> go t
    A.TLit l -> go l
    A.TCon qn -> do
      dt   <- liftTCM $ getConstructorData qn
      ctrs <- liftTCM $ getConstructors dt
      Just i <- pure $ qn `elemIndex` ctrs
      return $ LCtor (L.Inductive (qnameToKerName dt) 0) i [] 
      -- TODO(flupe): I *think* constructors have to be fully-applied
      -- TODO(flupe): mutual inductives
    A.TLet tt tu -> LLet <$> go tt <*> inBoundVar (go tu)
    A.TCase n A.CaseInfo{..} tt talts ->
      case caseErased of
        A.Erased _    -> fail "Erased matches are not supported."
        A.NotErased _ -> do
          calts <- traverse go talts
          cind  <- go caseType
          return $ LCase cind 0 (LRel n) calts
    A.TUnit -> return LBox
    A.TSort -> return LBox
    A.TErased -> return LBox
    A.TCoerce tt  -> fail "Coerces are not supported."
    A.TError terr -> fail "Errors are not supported."

instance A.TAlt ~> ([Name], L.Term) where
  go = \case
    A.TACon{..}   -> (take aArity $ repeat Anon,) <$> inBoundVars aArity (go aBody)
    A.TALit{..}   -> ([],)                        <$> go aBody
    A.TAGuard{..} -> fail "TAGuard"

instance A.CaseType ~> L.Inductive where
  go = \case
    A.CTData qn -> return $ L.Inductive (qnameToKerName qn) 0 
                   -- TODO(flupe): handle mutual inductive

    -- Builtin Nat
    A.CTNat -> do
      liftTCM (getBuiltinName builtinNat) >>= \case
        Nothing -> fail "Builtin Nat not bound."
        Just qn -> return $ L.Inductive (qnameToKerName qn) 0

    _           -> fail "Not supported case type"

-- TODO(flupe): handle using MetaCoq tPrim and prim_val
instance A.Literal ~> L.Term where
  go = \case
    LitNat n -> do
      Just qnat  <- liftTCM $ getBuiltinName builtinNat

      let indnat = L.Inductive (qnameToKerName qnat) 0

      return $ (!! fromInteger n) $ iterate (app (LCtor indnat 1 [])) (LCtor indnat 0 [])

    LitWord64 w -> fail "Literal int64 not supported"
    LitFloat  f -> fail "Literal float not supported"
    LitString s -> fail "Literal string not supported"
    LitChar   c -> fail "Literal char not supported"
    _           -> fail "Literal not supported"

instance A.TPrim ~> L.Term where
  go = const $ fail "unsupported prim"
