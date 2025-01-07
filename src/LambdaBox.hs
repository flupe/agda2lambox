{-# LANGUAGE OverloadedStrings #-}
module LambdaBox where

import Agda.Syntax.Common.Pretty
import System.FilePath ( FilePath )

type Ident = String
type KName = String

data Inductive = Inductive String Int
  deriving (Eq)

instance Show Inductive where
  show (Inductive s _) = show s

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

-- MetaCoq.Template.Kernames

type DirPath = [Ident]

-- | The module part of the kernel name
data ModPath 
  = MPFile DirPath
     -- ^ toplevel libraries i.e. .vo files
  | MPBound DirPath Ident Int
     -- ^ parameters of functors 
     -- NOTE(flupe): ????
  | MPDot ModPath Ident 
     -- ^ submodules
  deriving Show

-- | Absolute name of objects in the Coq kernel
data KerName = KerName 
  { kerModPath :: ModPath
  , kerName    :: Ident 
  } deriving Show

-- TODO(flupe): make this closer to the EAst defined in MetaCoq 1.3.2
data Term
  = LBox             -- ^ Proofs and erased terms
  | LRel Int         -- ^ Bound variable, with de Bruijn index
  | LVar Ident       -- ^ Free variable with identifier
      -- NOTE(flupe): not needed?
  | LLam Term        -- ^ Lambda abstraction
  | LLet Term Term  
      -- ^ Let bindings.
      --   Unused in the backend, since Agda itself no longer has let bindings
      --   in the concrete syntac.
  | LApp Term Term       -- ^ Term application
  | LConst KName         -- ^ Named constant.
  | LCtor Inductive Int  -- ^ Inductive constructor.
  | LCase Inductive Int Term [(Int, Term)]
      -- ^ Pattern-matching case construct.
  | LFix [Def] Int
     -- ^ Fixpoint combinator.
  deriving (Eq, Show)

data AllowedElims = IntoSProp | IntoPropSProp | IntoSetPropSProp | IntoAny
  deriving Show

data ConstructorBody = Ctor 
  { ctorName :: Ident
  , ctorArgs :: Int
  } deriving Show

data ProjectionBody = Proj
  { projName :: Ident
  , projArgs :: Int
  } deriving Show

-- | Inductive datatype declaration body
data OneInductiveBody = OneInductive
  { indName          :: Ident
  , indPropositional :: Bool
  , indKElim         :: AllowedElims
  , indCtors         :: [ConstructorBody]
  , indProjs         :: [ProjectionBody]
  } deriving Show

-- | Declaration of mutually defined inductive types
data MutualInductiveBody = MutualInductive
  { indPars   :: Int
  , indBodies :: [OneInductiveBody]
  } deriving Show

-- | Definition of a constant in the environment
type ConstantBody = Maybe Term

-- | Global declarations.
data GlobalDecl 
  = ConstantDecl ConstantBody
  | InductiveDecl MutualInductiveBody
  deriving Show

type GlobalDecls = [(KerName, GlobalDecl)]

instance Pretty Term where
  prettyPrec p v =
    case v of
      LBox    -> text "⫿"
      LRel x -> text $ "@" ++ show x
      LVar s -> text s
      LLam t ->
        mparens (p > 0) $
        sep [ "λ _ ->"
            , nest 2 (pretty t) ]
      LLet e t ->
        mparens (p > 0) $
        sep [ "let _ =" <+> nest 2 (pretty e)
            , pretty t ]
      LApp t t' ->
        mparens (p > 9) $
        sep [ pretty t
            , nest 2 $ prettyPrec 10 t' ]
      LConst s ->
        mparens (p > 0) $
        text s
      LCtor ind i ->
        pretty ind <> "#" <> pretty i
      LCase ind n t bs ->
        mparens (p > 0) $
        sep [ ("case<" <> pretty ind <> "," <> pretty n <> ">") <+> pretty t <+> "of"
            , nest 2 $ vcat (map (\(n, e) -> sep ["λ<" <> pretty n <> ">", nest 2 (pretty e)]) bs) ]
      LFix ds i -> -- FIXME: for mutual recursion
        mparens (p > 0) $
        sep [ "μ rec ->"
            , nest 2 (pretty $ ds !! i) ]

instance Pretty Inductive where
  prettyPrec _ (Inductive s _) = text s

instance Pretty Name where
  prettyPrec _ = \case
    Anon -> text "_"
    Named s -> text s
