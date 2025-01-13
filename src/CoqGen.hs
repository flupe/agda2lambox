{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
-- | Generating Coq code from our LambdaBox AST
module CoqGen where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda.Syntax.Common.Pretty
import LambdaBox
import Agda.Utils.Function (applyWhen)


-- | Wrapper for pretty-printing to Coq
newtype ToCoq a = ToCoq { unwrap :: a }

-- | Util to format Coq constructor with the given arguments.
ctor :: String -> [Doc] -> Doc
ctor = ctorP 0
{-# INLINE ctor #-}

-- | Util to format Coq constructor with the given arguments (and precedence).
ctorP :: Int -> String -> [Doc] -> Doc
ctorP p name []   = text name
ctorP p name args = applyWhen (p >= 10) parens $ text name <?> fsep args

-- | Util to format Coq record value with the given fields.
record :: [(String, Doc)] -> Doc
record = enclose
       . fsep
       . punctuate semi
       . map \(k, v) -> hang (text k <+> ":=") 2 v
  where enclose x = "{|" <+> x <+> "|}"

-- | Shorthand to generate Coq from a value.
pcoq :: Pretty (ToCoq a) => a -> Doc
pcoq = pcoqP 0
{-# INLINE pcoq #-}

-- | Shorthand to generate Coq from a value, given precedence.
pcoqP :: Pretty (ToCoq a) => Int -> a -> Doc
pcoqP p = prettyPrec p . ToCoq
{-# INLINE pcoqP #-}


instance {-# OVERLAPPING #-} Pretty (ToCoq String) where
  pretty (ToCoq s) = text (show s) <> "%bs"
  -- NOTE(flupe): "%s" to make sure that we produce Coq bytestrings

instance Pretty (ToCoq Int) where
  pretty = pretty . unwrap

instance Pretty (ToCoq Bool) where
  pretty (ToCoq v) = if v then "true" else "false"

instance Pretty (ToCoq a) => Pretty (ToCoq (Maybe a)) where
  prettyPrec p (ToCoq x) =
    case x of
      Nothing -> ctorP p "None" []
      Just y  -> ctorP p "Some" [pcoqP 10 y]

instance Pretty (ToCoq a) => Pretty (ToCoq [a]) where
  pretty (ToCoq xs) = brackets $ fsep (punctuate ";" $ map pcoq xs)

instance (Pretty (ToCoq a), Pretty (ToCoq b)) => Pretty (ToCoq (a, b)) where
  pretty (ToCoq (a, b)) = parens $ fsep [pcoq a <> comma, pcoq b]

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
      LBox             -> ctorP p "tBox"       []
      LRel k           -> ctorP p "tRel"       [pretty k]
      LVar s           -> ctorP p "tVar"       [pretty s]
      LLam t           -> ctorP p "tLambda"    [pcoq Anon, pcoqP 10 t]
      LLet e t         -> ctorP p "tLetIn"     [pcoq Anon, pcoqP 10 e, pcoqP 10 t]
      LApp u v         -> ctorP p "tApp"       [pcoqP 10 u, pcoqP 10 v]
      LConst c         -> ctorP p "tConst"     [pcoqP 10 c]
      LCtor ind i es   -> ctorP p "tConstruct" [pcoqP 10 ind, pcoqP 10 i, pcoqP 10 es]
      LCase ind n t bs -> ctorP p "tCase"      [pcoqP 10 (ind, n), pcoqP 10 t, pcoqP 10 bs]
      LFix mf i        -> ctorP p "tFix"       [pcoqP 10 mf, pcoqP 10 i]

instance Pretty (ToCoq Inductive) where
  pretty (ToCoq Inductive{..}) =
    record [ ("inductive_mind", pcoq indMInd)
           , ("inductive_ind",  pcoq indInd)
           ]

instance Pretty (ToCoq ModPath) where
  prettyPrec p (ToCoq mp) =
    case mp of
      MPFile dp       -> ctorP p "MPfile"  [pcoqP 10 dp]
      MPBound dp id i -> ctorP p "MPbound" [pcoqP 10 dp, pcoqP 10 id, pcoqP 10 i]
      MPDot mp id     -> ctorP p "MPdot"   [pcoqP 10 mp, pcoqP 10 id]

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
  prettyPrec p (ToCoq decl) =
    case decl of
      ConstantDecl  body  ->
        ctorP p "ConstantDecl"  [record [("cst_body", pcoq body)]]
      InductiveDecl minds ->
        ctorP p "InductiveDecl" [pcoqP 10 minds]

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
        [ "From Coq             Require Import List."
        , "From MetaCoq.Common  Require Import BasicAst Kernames Universes."
        , "From MetaCoq.Utils   Require Import bytestring."
        , "From MetaCoq.Erasure Require Import EAst."
        , "From Agda2Lambox     Require Import CheckWF Eval."
        , "Import ListNotations."
        ]

    , hang "Definition env : global_declarations :=" 2 $
        pcoq coqEnv <> "."

    , "Compute @check_wf_glob eflags env."

    , vsep $ flip map (zip [1..] $ reverse coqPrograms) \(i :: Int, kn) -> 
        let progname = "prog" <> pretty i in vsep
        [ hang ("Definition " <> progname <> " : program :=") 2 $
            pcoq (text "env" :: Doc, LConst kn) 
            <> "."
        , "Compute eval_program " <> progname <> "."
        ]
    ]
