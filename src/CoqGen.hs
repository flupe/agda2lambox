{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings, DataKinds, MonadComprehensions #-}
-- | Generating Coq code from our LambdaBox AST
module CoqGen where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda.Syntax.Common.Pretty
import LambdaBox
import Agda.Utils.Function (applyWhen)
import Agda2Lambox.Compile.Target


-- | Wrapper for pretty-printing to Coq
data ToCoq t a = ToCoq (Target t) a

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
pcoq :: Pretty (ToCoq t a) => Target t -> a -> Doc
pcoq t = pcoqP t 0
{-# INLINE pcoq #-}

-- | Shorthand to generate Coq from a value, given precedence.
pcoqP :: Pretty (ToCoq t a) => Target t -> Int -> a -> Doc
pcoqP t p = prettyPrec p . ToCoq t
{-# INLINE pcoqP #-}


instance Pretty (ToCoq t Doc) where
  pretty (ToCoq _ d) = d

instance {-# OVERLAPPING #-} Pretty (ToCoq t String) where
  pretty (ToCoq _ s) = text (show s) <> "%bs"
  -- NOTE(flupe): "%bs" to make sure that we produce Coq bytestrings

instance Pretty (ToCoq t Int) where
  pretty (ToCoq _ s) = pretty s

instance Pretty (ToCoq t Bool) where
  pretty (ToCoq t v) = if v then "true" else "false"

instance Pretty (ToCoq t a) => Pretty (ToCoq t (Maybe a)) where
  prettyPrec p (ToCoq t x) =
    case x of
      Nothing -> ctorP p "None" []
      Just y  -> ctorP p "Some" [pcoqP t 10 y]

instance Pretty (ToCoq t a) => Pretty (ToCoq t [a]) where
  pretty (ToCoq t xs) =
    brackets $ fsep (punctuate ";" $ map (pcoq t) xs)

instance (Pretty (ToCoq t a), Pretty (ToCoq t b)) => Pretty (ToCoq t (a, b)) where
  pretty (ToCoq t (a, b)) = parens $ fsep [pcoq t a <> comma, pcoq t b]

instance Pretty (ToCoq t Name) where
  pretty (ToCoq t n) =
    case n of
      Anon    -> ctor "nAnon"  []
      Named i -> ctor "nNamed" [pcoq t i]

instance Pretty (ToCoq t Inductive) where
  pretty (ToCoq t Inductive{..}) =
    record [ ("inductive_mind", pcoq t indMInd)
           , ("inductive_ind",  pcoq t indInd)
           ]

instance Pretty (ToCoq t ModPath) where
  prettyPrec p (ToCoq t mp) =
    case mp of
      MPFile dp       -> ctorP p "MPfile"  [pcoqP t 10 dp]
      MPBound dp id i -> ctorP p "MPbound" [pcoqP t 10 dp, pcoqP t 10 id, pcoqP t 10 i]
      MPDot mp id     -> ctorP p "MPdot"   [pcoqP t 10 mp, pcoqP t 10 id]

instance Pretty (ToCoq t KerName) where
  pretty (ToCoq t KerName{..}) = pcoq t (kerModPath, kerName)

instance Pretty (ToCoq t d) => Pretty (ToCoq t (Def d)) where
  pretty (ToCoq t Def{..}) =
    record [ ("dname", pcoq t dName)
           , ("dbody", pcoq t dBody)
           , ("rarg",  pcoq t dArgs)
           ]

instance Pretty (ToCoq t Term) where
  prettyPrec p (ToCoq t v) =
    case v of
      LBox                -> ctorP p "tBox"       []
      LRel k              -> ctorP p "tRel"       [pretty k]
      LLambda n u         -> ctorP p "tLambda"    [pcoq t n, pcoqP t 10 u]
      LLetIn n u v        -> ctorP p "tLetIn"     [pcoq t n, pcoqP t 10 u, pcoqP t 10 v]
      LApp u v            -> ctorP p "tApp"       [pcoqP t 10 u, pcoqP t 10 v]
      LConst c            -> ctorP p "tConst"     [pcoqP t 10 c]
      LConstruct ind i es -> ctorP p "tConstruct" [pcoqP t 10 ind, pcoqP t 10 i, pcoqP t 10 es]
      LCase ind n u bs    -> ctorP p "tCase"      [pcoqP t 10 (ind, n), pcoqP t 10 u, pcoqP t 10 bs]
      LFix mf i           -> ctorP p "tFix"       [pcoqP t 10 mf, pcoqP t 10 i]

instance Pretty (ToCoq t Type) where
  prettyPrec p (ToCoq t v) =
    case v of
      TBox      -> ctorP p "TBox"   []
      TAny      -> ctorP p "TAny"   []
      TArr a b  -> ctorP p "TArr"   [pcoqP t 10 a, pcoqP t 10 b]
      TApp a b  -> ctorP p "TApp"   [pcoqP t 10 a, pcoqP t 10 b]
      TVar k    -> ctorP p "TVar"   [pretty k]
      TInd ind  -> ctorP p "TInd"   [pcoqP t 10 ind]
      TConst kn -> ctorP p "TConst" [pcoqP t 10 kn]

instance Pretty (ToCoq t RecursivityKind) where
  pretty (ToCoq _ rk) =
    case rk of
      Finite   -> ctor "Finite"   []
      CoFinite -> ctor "CoFinite" []
      BiFinite -> ctor "BiFinite" []

instance Pretty (ToCoq t AllowedElims) where
  pretty (ToCoq t ae) =
    case ae of
      IntoSProp        -> ctor "IntoSProp"        []
      IntoPropSProp    -> ctor "IntoPropSProp"    []
      IntoSetPropSProp -> ctor "IntoSetPropSProp" []
      IntoAny          -> ctor "IntoAny"          []

instance Pretty (ToCoq t (ConstructorBody t)) where
  pretty (ToCoq t Constructor{..}) =
    case t of
      ToUntyped ->
        record [ ("cstr_name",  pcoq t cstrName)
               , ("cstr_nargs", pcoq t cstrArgs)
               ]
      ToTyped   ->
        pcoq t (pcoq t cstrName, (pcoq t (extract cstrTypes), pcoq t cstrArgs))

instance Pretty (ToCoq t (ProjectionBody t)) where
  pretty (ToCoq t Projection{..}) =
    case t of
      ToUntyped -> record [ ("proj_name",  pcoq t projName) ]
      ToTyped   -> pcoq t (pcoq t projName, pcoq t $ extract projType)

instance Pretty (ToCoq t TypeVarInfo) where
  pretty (ToCoq t TypeVarInfo{..}) =
    record
      [ ("tvar_name",       pcoq t tvarName)
      , ("tvar_is_logical", pcoq t tvarIsLogical)
      , ("tvar_is_arity",   pcoq t tvarIsArity)
      , ("tvar_is_sort",    pcoq t tvarIsSort)
      ]

instance Pretty (ToCoq t (OneInductiveBody t)) where
  pretty (ToCoq t OneInductive{..}) =
    record $
      [ ("ind_name",          pcoq t indName)
      , ("ind_propositional", pcoq t indPropositional)
      , ("ind_kelim",         pcoq t indKElim)
      , ("ind_ctors",         pcoq t indCtors)
      , ("ind_projs",         pcoq t indProjs)
      ] ++
      case t of
        ToUntyped -> []
        ToTyped   -> [("ind_type_vars", pcoq t $ extract indTypeVars)]

instance Pretty (ToCoq t (MutualInductiveBody t)) where
  pretty (ToCoq t MutualInductive{..}) =
    record [ ("ind_finite", pcoq t indFinite)
           , ("ind_npars",  pcoq t indPars)
           , ("ind_bodies", pcoq t indBodies)
           ]

instance Pretty (ToCoq t (ConstantBody t)) where
  prettyPrec p (ToCoq t ConstantBody{..}) =
    record $
      [("cst_body", pcoq t cstBody)] ++
      case t of
        ToUntyped -> []
        ToTyped   -> [("cst_type", pcoq t $ extract cstType)]

instance Pretty (ToCoq t (GlobalDecl t)) where
  prettyPrec p (ToCoq t decl) =
    case decl of
      ConstantDecl  body  ->
        ctorP p "ConstantDecl"  [pcoqP t 10 body]
      InductiveDecl minds ->
        ctorP p "InductiveDecl" [pcoqP t 10 minds]
      TypeAliasDecl typ ->
        ctorP p "TypeAliasDecl" [pcoqP t 10 typ]

instance Pretty (ToCoq t (GlobalEnv t)) where
  pretty (ToCoq t (GlobalEnv env)) =
    case t of
      ToUntyped -> pcoq t env
      ToTyped   -> pcoq t $ flip map env \(kn, decl) -> (kn, (True, decl))

instance Pretty (ToCoq t (CoqModule t)) where
  pretty (ToCoq t CoqModule{..}) = vsep
    [ vcat
        [ "From Coq             Require Import List."
        , "From MetaCoq.Common  Require Import BasicAst Kernames Universes."
        , "From MetaCoq.Utils   Require Import bytestring."
        , "From MetaCoq.Erasure Require Import EAst."
        , "From Agda2Lambox     Require Import CheckWF Eval."
        , "Import ListNotations."
        ]

    , hang "Definition env : global_declarations :=" 2 $
        pcoq t coqEnv <> "."

    , "Compute @check_wf_glob eflags env."

    , vsep $ flip map (zip [1..] $ reverse coqPrograms) \(i :: Int, kn) -> 
        let progname = "prog" <> pretty i in vsep
        [ hang ("Definition " <> progname <> " : program :=") 2 $
            pcoq t (text "env" :: Doc, LConst kn) 
            <> "."
        , "Compute eval_program " <> progname <> "."
        ]
    ]
