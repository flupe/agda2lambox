{-# LANGUAGE OverloadedStrings , LambdaCase #-}
module LambdaBox where

-- import qualified Agda as A
import Agda.Syntax.Common.Pretty

type Ident = String
type KName = String

newtype Inductive = Inductive String
  deriving (Eq)

instance Show Inductive where
  show (Inductive s) = show s

data Name = Anon | Named Ident
  deriving (Eq, Show)

data Def = Def Name Term Int
  deriving (Eq, Show)

instance Pretty Def where
  -- prettyPrec _ (Def s _ _) = pretty s
  prettyPrec _ (Def _ t _) = pretty t

-- Taken from:
-- https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/ExAst.v
data Type
  = TBox
  | TAny
  | TArr Type Type
  | TApp Type Type

  -- According to the Coq file, the int is the "Level of type variable".
  -- Unclear whether this would be a De Bruijn level or universe level.
  | TVar Int
  | TInd Inductive
  | TConst KName
  deriving (Eq , Show)

instance Pretty Type where
  prettyPrec p v =
    case v of
      TBox ->
        text "□"
      TAny ->
        text "⁇"
      TArr s t ->
        mparens (p > 0) $
          prettyPrec 1 s <+> "->" <+> pretty t
      TApp s t ->
        mparens (p > 9) $ pretty s <+> prettyPrec 10 t
      TVar n ->
        text ("@" <> show n)
      TInd ind ->
        mparens (p > 0) $
          "<" <> pretty ind <> ">"
      TConst kname ->
        text kname


data Term
  = Box             -- ^ Proofs and erased terms
  | Rel Int         -- ^ Bound variable, with de Bruijn index
  | Var Ident       -- ^ Free variable with identifier
  | Lam Name Term   -- ^ Lambda abstraction
  | Let Name Term Term  
      -- ^ Let bindings.
      --   Unused in the backend, since Agda itself no longer has let bindings
      --   in the concrete syntac.
  | App Term Term       -- ^ Term application
  | Const KName         -- ^ Named constant.
  | Ctor Inductive Int  -- ^ Inductive constructor.
  | Case Inductive Int Term [(Int, Term)]
      -- ^ Pattern-matching case construct.
  | Fix [Def] Int
     -- ^ Fixpoint combinator.
     -- NOTE(flupe): Why no terms?
  deriving (Eq, Show)


instance Pretty Term where
  prettyPrec p v =
    case v of
      Box    -> text "⫿"
      Rel x -> text $ "@" ++ show x
      Var s -> text s
      Lam s t ->
        mparens (p > 0) $
        sep [ "λ" <+> pretty s <+> "->"
            , nest 2 (pretty t) ]
      Let x e t ->
        mparens (p > 0) $
        sep [ "let" <+> sep [ pretty x <+> "="
                            , nest 2 (pretty e) ]
            , pretty t ]
      App t t' ->
        mparens (p > 9) $
        sep [ pretty t
            , nest 2 $ prettyPrec 10 t' ]
      Const s ->
        mparens (p > 0) $
        text s
      Ctor ind i ->
        pretty ind <> "#" <> pretty i
      Case ind n t bs ->
        mparens (p > 0) $
        sep [ ("case<" <> pretty ind <> "," <> pretty n <> ">") <+> pretty t <+> "of"
            , nest 2 $ vcat (map (\(n, e) -> sep ["λ<" <> pretty n <> ">", nest 2 (pretty e)]) bs) ]
      Fix ds i -> -- FIXME: for mutual recursion
        mparens (p > 0) $
        sep [ "μ rec ->"
            , nest 2 (pretty $ ds !! i) ]


instance Pretty Inductive where
  prettyPrec _ (Inductive s) = text s

instance Pretty Name where
  prettyPrec _ = \case
    Anon -> text "_"
    Named s -> text s
