{-# LANGUAGE OverloadedStrings #-}
-- | Haskell encoding of LambdaBox terms.
-- The encoding attempts staying as faithful as possible
-- to the corresponding MetaCoq encoding.
module LambdaBox where

import Agda.Syntax.Common.Pretty
import Agda.Syntax.Abstract.Name ( QName(..), ModuleName(..) )
import Agda.Syntax.Common.Pretty ( prettyShow )

-- TODO(flupe): Record projections?

-- | Identifiers.
-- Used only for pretty-printing by MetaCoq?
-- MetaCoq uses bytestrings, it doesn't matter here.
type Ident = String

-- | Reference to an inductive datatype.
data Inductive = Inductive
  { indMInd :: KerName
      -- ^ The kernel name of the mututal inductives it comes from.
  , indInd  :: Int
      -- ^ Which of those is the inductive we care about.
  } deriving (Eq, Show)


data Name = Anon | Named Ident
  deriving (Eq, Show)


type DirPath = [Ident]

-- | The module part of the kernel name
data ModPath 
  = MPFile DirPath
     -- ^ toplevel libraries i.e. .vo files
  | MPBound DirPath Ident Int -- NOTE(flupe): ????
     -- ^ parameters of functors 

  | MPDot ModPath Ident 
     -- ^ submodules
  deriving (Eq, Show)

instance Pretty ModPath where
  pretty = \case
    MPFile  dp      -> cat $ punctuate "." (map pretty dp)
    MPBound dp id i -> "MPBound"
    MPDot mp i      -> pretty mp <> "." <> pretty i


-- | Absolute name of objects in the Coq kernel
data KerName = KerName 
  { kerModPath :: ModPath
  , kerName    :: Ident 
  } deriving (Eq, Show)

instance Pretty KerName where
  pretty KerName{..} = pretty kerModPath <> "." <> text kerName


-- TODO(flupe): double check that this name conversion is sound
-- TODO(flupe): move this somewhere appropriate

modNameToModPath :: ModuleName -> ModPath
modNameToModPath MName{..} = MPFile $ map prettyShow mnameToList

qnameToKerName :: QName -> KerName
qnameToKerName QName{..} =
  KerName (modNameToModPath qnameModule) 
          (prettyShow qnameName)


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
  | TConst KerName
  deriving (Eq , Show)

instance Pretty Type where
  prettyPrec p = \case
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
      pretty kname


-- | Definition in a mutual fixpoint.
data Def t = Def
  { dName :: Name
  , dBody :: t
  , dArgs :: Int
  } deriving (Eq, Show)

instance Pretty t => Pretty (Def t) where
  -- prettyPrec _ (Def s _ _) = pretty s
  prettyPrec _ (Def _ t _) = pretty t

-- | Mutual-defined fixpoints.
type MFixpoint = [Def Term]

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
  | LApp Term Term              -- ^ Term application
  | LConst KerName              -- ^ Named constant.
    -- TODO(flupe): I *think* constructors have to be fully-applied.
  | LCtor Inductive Int [Term]  -- ^ Inductive constructor.
  | LCase Inductive Int Term [([Name], Term)]
      -- ^ Pattern-matching case construct.
  | LFix MFixpoint Int
      -- ^ Fixpoint combinator. 
      -- The index refers to which fixpoint we select out of
      -- the generated (mutual) fixpoints.
  deriving (Eq, Show)

data AllowedElims
  = IntoSProp
  | IntoPropSProp
  | IntoSetPropSProp
  | IntoAny
  deriving (Eq, Show)

data ConstructorBody = Ctor 
  { ctorName :: Ident
  , ctorArgs :: Int
  } deriving (Eq, Show)

data ProjectionBody = Proj
  { projName :: Ident
  , projArgs :: Int
  } deriving (Eq, Show)

-- | Inductive datatype declaration body
data OneInductiveBody = OneInductive
  { indName          :: Ident
  , indPropositional :: Bool
  , indKElim         :: AllowedElims
  , indCtors         :: [ConstructorBody]
  , indProjs         :: [ProjectionBody]
  } deriving (Eq, Show)

-- | Declaration of mutually defined inductive types
data MutualInductiveBody = MutualInductive
  { indPars   :: Int
  , indBodies :: [OneInductiveBody]
  } deriving (Eq, Show)

-- | Definition of a constant in the environment
type ConstantBody = Maybe Term

-- | Global declarations.
data GlobalDecl 
  = ConstantDecl ConstantBody
  | InductiveDecl MutualInductiveBody
  deriving (Eq, Show)

type GlobalDecls = [(KerName, GlobalDecl)]

instance Pretty Term where
  prettyPrec p v =
    case v of
      LBox   -> text "⫿"
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
        pretty s
      LCtor ind i es ->
        sep [ pretty ind <> "#" <> pretty i
            , nest 2 $ sep $ map (prettyPrec 10) es
            ]
      LCase ind n t bs ->
        mparens (p > 0) $
        sep [ ("case<" <> pretty ind <> "," <> pretty n <> ">") <+> pretty t <+> "of"
            , nest 2 $ vcat (map (\(n, e) -> sep ["λ<" <> pretty n <> ">", nest 2 (pretty e)]) bs) ]
      LFix ds i -> -- FIXME: for mutual recursion
        mparens (p > 0) $
        sep [ "μ rec ->"
            , nest 2 (pretty $ ds !! i) ]

instance Pretty Inductive where
  pretty Inductive{..} = pretty indMInd <> "." <> pretty indInd

instance Pretty Name where
  prettyPrec _ = \case
    Anon -> text "_"
    Named s -> text s
