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

instance Pretty (ToCoq Bool) where
  pretty (ToCoq v) = if v then "true" else "false"

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

instance Pretty (ToCoq Doc) where
  pretty (ToCoq d) = d

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

instance Pretty (ToCoq RecursivityKind) where
  pretty (ToCoq rk) =
    case rk of
      Finite   -> ctor "Finite"   []
      CoFinite -> ctor "CoFinite" []
      BiFinite -> ctor "BiFinite" []

instance Pretty (ToCoq AllowedElims) where
  pretty (ToCoq ae) =
    case ae of
      IntoSProp        -> ctor "IntoSProp"        []
      IntoPropSProp    -> ctor "IntoPropSProp"    []
      IntoSetPropSProp -> ctor "IntoSetPropSProp" []
      IntoAny          -> ctor "IntoAny"          []

instance Pretty (ToCoq ConstructorBody) where
  pretty (ToCoq Ctor{..}) =
    record [ ("cstr_name",  pcoq ctorName)
           , ("cstr_nargs", pcoq ctorArgs)
           ]

instance Pretty (ToCoq ProjectionBody) where
  pretty (ToCoq Proj{..}) =
    record [ ("proj_name",  pcoq projName)
           ]

instance Pretty (ToCoq OneInductiveBody) where
  pretty (ToCoq OneInductive{..}) =
    record [ ("ind_name",          pcoq indName)
           , ("ind_propositional", pcoq indPropositional)
           , ("ind_kelim",         pcoq indKElim)
           , ("ind_ctors",         pcoq indCtors)
           , ("ind_projs",         pcoq indProjs)
           ]

instance Pretty (ToCoq MutualInductiveBody) where
  pretty (ToCoq MutualInductive{..}) =
    record [ ("ind_finite", pcoq indFinite)
           , ("ind_npars",  pcoq indPars)
           , ("ind_bodies", pcoq indBodies)
           ]

instance Pretty (ToCoq GlobalDecl) where
  pretty (ToCoq decl) =
    case decl of
      ConstantDecl  body  ->
        ctor "ConstantDecl" [record [("cst_body", pcoq body)]]
      InductiveDecl minds ->
        ctor "InductiveDecl" [pcoq minds]

instance Pretty (ToCoq t) => Pretty (ToCoq (Def t)) where
  pretty (ToCoq Def{..}) =
    record [ ("dname", pcoq dName)
           , ("dbody", pcoq dBody)
           , ("rarg",  pcoq dArgs)
           ]

-- | Generated module
data CoqModule = CoqModule
  { coqEnv      :: [(KerName, GlobalDecl)]
  , coqPrograms :: [KerName]
  }

instance Pretty (ToCoq CoqModule) where
  pretty (ToCoq CoqModule{..}) = vsep
    [ vcat
        [ "From MetaCoq.Common Require Import BasicAst Kernames Universes."
        , "From MetaCoq.Erasure Require Import EAst."
        , "From MetaCoq.Utils Require Import bytestring MCString."
        , "From Coq Require Import List."
        , ""
        , "Import ListNotations."
        , "Open Scope pair_scope."
        , "Open Scope bs."
        ]
    , hang "Definition env : global_declarations :=" 2 $ pcoq coqEnv <> "."
    , vsep $ flip map (zip [1..] coqPrograms) \(i :: Int, kn) ->
        hang ("Definition prog" <> pretty i <> " : program :=") 2 $
          pcoq (text "env" :: Doc, LConst kn) 
          <> "."
    ]
