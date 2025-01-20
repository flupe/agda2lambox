{-# LANGUAGE OverloadedStrings #-}
-- | Definition of λ□ types.
module LambdaBox.Type where

import Agda.Syntax.Common.Pretty
import LambdaBox.Names

data Type
  = TBox
  | TAny
  | TArr Type Type
  | TApp Type Type
  -- NOTE(Cas):
  -- According to the Coq file, the int is the "Level of type variable".
  -- Unclear whether this would be a De Bruijn level or universe level.
  | TVar Int
  | TInd Inductive
  | TConst KerName

instance Pretty Type where
  prettyPrec p = \case
    TBox         -> "□"
    TAny         -> "*"
    TArr s t     -> mparens (p > 0) $ prettyPrec 1 s <+> "→" <+> pretty t
    TApp s t     -> mparens (p > 9) $ pretty s <+> prettyPrec 10 t
    TVar n       -> "@" <> pretty n
    TInd ind     -> mparens (p > 0) $ pretty ind
    TConst kname -> pretty kname
