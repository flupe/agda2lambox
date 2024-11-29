{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Convert.Terms () where

import Utils

import qualified Agda as A
import Agda.Lib ( )
import Agda.Utils

import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class
-- import Agda2Lambox.Convert.Names
-- import Agda2Lambox.Convert.Literals
-- import Agda2Lambox.Convert.Builtins
-- import Agda2Lambox.Convert.Types

fixme :: L.Name
fixme = L.Named "FIXME"

-- | Compiling (treeless) Agda terms into Lambox expressions.
instance A.TTerm ~> L.Term where
  go = \case
    A.TVar n -> return $ L.BVar n -- Can variables be erased?
    A.TPrim tp -> return $ L.Const (show tp) -- FIXME
    A.TDef qn ->
      inDefName qn
        (return $ L.FVar (A.prettyShow qn))
        \ix -> return $ L.BVar ix
    A.TApp t args -> do
      ct <- go t
      cargs <- mapM go args
      return $ foldl L.App ct cargs
    A.TLam tt -> inBoundVar $
      L.Lam L.Anon <$> go tt
    A.TLit l -> return $ L.Const (show l) -- FIXME: How are literals (eg. the numeral 10) represented in λ□?
    A.TCon qn -> return $ L.Ctor (L.Inductive $ A.prettyShow qn) 99 -- FIXME
    A.TLet tt tu -> L.Let L.Anon <$> go tt <*> (go tu) -- FIXME: name
    A.TCase n A.CaseInfo{..} tt talts ->
      case caseErased of
        A.Erased _ -> fail ""
        A.NotErased _ -> do
          calts <- traverse go talts
          cind <- go caseType
          return $ L.Case cind 0 (L.BVar n) calts

    A.TUnit -> return L.Box
    A.TSort -> return L.Box
    A.TErased -> return L.Box
    A.TCoerce tt -> fail "TCoerce"
    A.TError terr -> fail "TError"

instance A.TAlt ~> (Int, L.Term) where
  go = \case
    A.TACon{..} -> (aArity,) <$> go aBody
    A.TALit{..} -> (0,) <$> go aBody
    A.TAGuard{..} -> fail "TAGuard"

instance A.CaseType ~> L.Inductive where
  go = \case
    A.CTData qn -> return $ L.Inductive (A.prettyShow qn)
    A.CTNat -> return $ L.Inductive "Nat"
    _ -> fail ""
