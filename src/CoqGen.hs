{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Generating Coq code from our LambdaBox AST
module CoqGen where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda.Syntax.Common.Pretty
import LambdaBox


-- TODO(flupe): conditionally insert parentheses


-- | Wrapper for pretty-printing to Coq
newtype ToCoq a = ToCoq { unwrap :: a }


-- | Util to format Coq constructor with the given arguments.
ctor :: String -> [Doc] -> Doc
ctor name []   = text name
ctor name args = parens $ hsep (text name : args)

-- | Util to format Coq record value with the given fields.
record :: [(String, Doc)] -> Doc
record = enclose
       . sep
       . punctuate semi
       . map \(k, v) -> text k <+> ":=" <+> v
  where enclose x = "{|" <+> x <+> "|}"


pcoq :: Pretty (ToCoq a) => a -> Doc
pcoq = pretty . ToCoq
{-# INLINE pcoq #-}


instance {-# OVERLAPPING #-} Pretty (ToCoq String) where
  pretty (ToCoq s) = text $ show s

instance Pretty (ToCoq Int) where
  pretty = pretty . unwrap

instance Pretty (ToCoq a) => Pretty (ToCoq (Maybe a)) where
  pretty (ToCoq x) =
    case x of
      Nothing -> ctor "None" []
      Just y  -> ctor "Some" [pcoq y]

instance Pretty (ToCoq a) => Pretty (ToCoq [a]) where
  pretty (ToCoq xs) = brackets $ fsep (punctuate ";" $ map pcoq xs)

instance (Pretty (ToCoq a), Pretty (ToCoq b)) => Pretty (ToCoq (a, b)) where
  pretty (ToCoq (a, b)) = parens $ (pcoq a <> comma) <+> pcoq b

instance Pretty (ToCoq Name) where
  pretty (ToCoq n) =
    case n of 
      Anon    -> ctor "nAnon"  []
      Named i -> ctor "nNamed" [pcoq i]

instance Pretty (ToCoq Term) where
  prettyPrec p (ToCoq v) =
    case v of
      LBox             -> ctor "tBox"       []
      LRel k           -> ctor "tRel"       [pretty k]
      LVar s           -> ctor "tVar"       [pretty s]
      LLam t           -> ctor "tLambda"    [pcoq Anon, pcoq t]
      LLet e t         -> ctor "tLetIn"     [pcoq Anon, pcoq e, pcoq t]
      LApp u v         -> ctor "tApp"       [pcoq u, pcoq v]
      LConst c         -> ctor "tConst"     [pcoq c]
      LCtor ind i es   -> ctor "tConstruct" [pcoq ind, pcoq i, pcoq es]
      LCase ind n t bs -> ctor "tCase"      [pcoq (ind, n), pcoq t, pcoq bs]
      LFix mf i        -> ctor "tFix"       [pcoq mf, pcoq i]

instance Pretty (ToCoq Inductive) where
  pretty (ToCoq Inductive{..}) =
    record [ ("inductive_mind", pcoq indMInd)
           , ("inductive_ind",  pcoq indInd)
           ]

instance Pretty (ToCoq ModPath) where
  pretty (ToCoq p) =
    case p of
      MPFile dp       -> ctor "MPfile"  [pcoq dp]
      MPBound dp id i -> ctor "MPbound" [pcoq dp, pcoq id, pcoq i]
      MPDot mp id     -> ctor "MPdot"   [pcoq mp, pcoq id]

instance Pretty (ToCoq KerName) where
  pretty (ToCoq KerName{..}) = pcoq (kerModPath, kerName)

instance Pretty (ToCoq GlobalDecl) where
  pretty (ToCoq decl) =
    case decl of
      ConstantDecl  body  ->
        ctor "ConstantDecl" [record [("cst_body", pcoq body)]]
      InductiveDecl minds ->
        ctor "InductiveDecl" []

instance Pretty (ToCoq t) => Pretty (ToCoq (Def t)) where
  pretty (ToCoq Def{..}) =
    record [ ("dname", pcoq dName)
           , ("dbody", pcoq dBody)
           , ("rarg",  pcoq dArgs)
           ]
