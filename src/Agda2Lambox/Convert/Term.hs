{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Convert.Term () where

import Control.Monad.Reader ( ask, liftIO )
import Data.List ( elemIndex )

import Utils

import Agda ( liftTCM, getConstructorData, getConstructors )
import qualified Agda as A
import Agda.Lib ( )
import Agda.Utils
import Agda.Syntax.Literal

import LambdaBox (Term(..))
import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class


-- | Compiling (treeless) Agda terms into Lambox expressions.
instance A.TTerm ~> L.Term where
  go = \case
    A.TVar n   -> return $ LRel n
    A.TPrim pr -> go pr 
    A.TDef qn -> do
      Env{..} <- ask
      return case qn `elemIndex` mutuals of
        Nothing -> LVar (unqual qn)
        Just i  -> LRel (i + boundVars) -- NOTE(flupe): this looks fishy
                                         --              this isn't a (locally-bound) var
                                         --              but a constant?
    A.TApp t args -> do
      ct    <- go t
      cargs <- mapM go args
      return $ foldl LApp ct cargs
    A.TLam t -> inBoundVar $ LLam <$> go t
    A.TLit l -> go l
    A.TCon qn -> do
      dt   <- liftTCM $ getConstructorData qn
      ctrs <- liftTCM $ getConstructors dt
      Just i <- pure $ qn `elemIndex` ctrs
      return $ LCtor (L.Inductive (unqual dt) 0) i -- TODO(flupe) mutual inductives
    A.TLet tt tu -> LLet <$> go tt <*> inBoundVar (go tu)
    A.TCase n A.CaseInfo{..} tt talts ->
      case caseErased of
        A.Erased _ -> fail "Erased matches are not supported."
        A.NotErased _ -> do
          calts <- traverse go talts
          cind <- go caseType
          return $ LCase cind 0 (LRel n) calts
    A.TUnit -> return LBox
    A.TSort -> return LBox
    A.TErased -> return LBox
    A.TCoerce tt  -> fail "Coerces are not supported."
    A.TError terr -> fail "Errors are not supported."

instance A.TAlt ~> (Int, L.Term) where
  go = \case
    A.TACon{..} -> (aArity,) <$> inBoundVars aArity (go aBody)
    A.TALit{..} -> (0,) <$> go aBody
    A.TAGuard{..} -> fail "TAGuard"

instance A.CaseType ~> L.Inductive where
  go = \case
    A.CTData qn -> return $ L.Inductive (unqual qn) 0 -- TODO(flupe): handle mutual inductive
    A.CTNat -> return $ L.Inductive "Nat" 0           -- TODO(flupe): idem
    _ -> fail ""

-- TODO(flupe): handle using MetaCoq tPrim and prim_val
instance A.Literal ~> L.Term where
  go = \case
    LitNat    n -> fail "Literal natural numbers not supported"
    LitWord64 w -> fail "Literal int64 not supported"
    LitFloat  f -> fail "Literal float not supported"
    LitString s -> fail "Literal string not supported"
    LitChar   c -> fail "Literal char not supported"
    _           -> fail "Literal not supported"

instance A.TPrim ~> L.Term where
  go = \case
    A.PAdd -> return $ LConst "Nat.add"
    A.PMul -> return $ LConst "Nat.mult"
    _ -> fail ""
