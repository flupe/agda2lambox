{-# LANGUAGE LambdaCase, FlexibleInstances #-}
module Agda2Lambox.Convert.Type () where


import Utils

import qualified Agda as A
import Agda.Lib ( )
import Agda.Utils

import qualified LambdaBox as L

import Agda2Lambox.Monad
import Agda2Lambox.Convert.Class


instance A.Type ~> L.Type where
  go = \case
    A.El sort t ->
      convert t    

-- | Erasure of types in MetaCoq is defined in the following location:
-- https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/Erasure.v#L765
instance A.Term ~> L.Type where 
  go = \case
    A.Var n elims ->
      foldl
        (\m elim ->
           L.TApp <$> m <*> convert elim
        )  (return $ L.TVar n) elims
    A.Lam _ _ ->
      fail "Cannot convert type-level abstractions to a λ□ type."
    A.Lit _ ->
      fail "Cannot convert type-level literals to a λ□ type."
    A.Def qname elims -> 
      foldl
        (\m elim ->
          L.TApp <$> m <*> (convert elim)
        ) (return $ L.TConst $ pp qname) elims
    A.Con _ _ _ ->
      fail "Cannot convert data constructor to a λ□ type."
    A.Pi dom abs -> do
      L.TArr <$> convert (A.unDom dom) <*> convert abs
    A.Sort _ -> return L.TBox
    A.Level _ -> return L.TBox
    t ->
      fail $ "Unsupported type:" <> show t

instance A.Abs A.Type ~> L.Type where
  go = \case
    A.Abs name body ->
      convert body 
    A.NoAbs name body ->
      convert body 

instance A.Elim ~> L.Type where
  go = \case
    A.Apply a ->
      convert $ A.unArg a
    A.Proj _ _ ->
      fail "Cannot convert projection elims"
    A.IApply _ _ _ ->
      fail "Cannot convert path elim" 
      

    {-
= Var {-# UNPACK #-} !Int Elims -- ^ @x es@ neutral
          | Lam ArgInfo (Abs Term)        -- ^ Terms are beta normal. Relevance is ignored
          | Lit Literal
          | Def QName Elims               -- ^ @f es@, possibly a delta/iota-redex
          | Con ConHead ConInfo Elims
          -- ^ @c es@ or @record { fs = es }@
          --   @es@ allows only Apply and IApply eliminations,
          --   and IApply only for data constructors.
          | Pi (Dom Type) (Abs Type)      -- ^ dependent or non-dependent function space
          | Sort Sort
          | Level Level
          | MetaV {-# UNPACK #-} !MetaId Elims
          | DontCare Term
            -- ^ Irrelevant stuff in relevant position, but created
            --   in an irrelevant context.  Basically, an internal
            --   version of the irrelevance axiom @.irrAx : .A -> A@.
          | Dummy String Elims
            -- ^ A (part of a) term or type which is only used for internal purposes.
            --   Replaces the @Sort Prop@ hack.
            --   The @String@ typically describes the location where we create this dummy,
            --   but can contain other information as well.
            --   The second field accumulates eliminations in case we
            --   apply a dummy term to more of them. Dummy terms should never be used in places
            --   where they can affect type checking, so syntactic checks are free to ignore the
            --   eliminators, which are only there to ease debugging when a dummy term incorrectly
            --   leaks into a relevant position.

-} 
