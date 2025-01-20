{-# LANGUAGE DataKinds, GADTs, OverloadedStrings #-}
module LambdaBox.Env where

import Data.Kind ( Type )

import Agda.Syntax.Common.Pretty

import LambdaBox.Names
import LambdaBox.Term
import LambdaBox.Type qualified as LBox
import Agda2Lambox.Compile.Target

-- Allowed elimination target for a datatype.
data AllowedElims
  = IntoSProp
  | IntoPropSProp
  | IntoSetPropSProp
  | IntoAny

data RecursivityKind
  = Finite   -- ^ Inductive datatype.
  | CoFinite -- ^ Coinductive datatype.
  | BiFinite -- ^ Non-recursive.

-- | Constructor info in datatype declaration.
data ConstructorBody t = Constructor
  { cstrName  :: Ident
  , cstrArgs  :: Int
  , cstrTypes :: WhenTyped [(Name, LBox.Type)] t
  }

-- | Projection info in datatype declaration.
data ProjectionBody t = Projection
  { projName :: Ident
  , projType :: WhenTyped LBox.Type t
  }

data TypeVarInfo = TypeVarInfo
  { tvarName      :: Name
  , tvarIsLogical :: Bool
  , tvarIsArity   :: Bool
  , tvarIsSort    :: Bool
  }

-- | Inductive datatype declaration body
data OneInductiveBody t = OneInductive
  { indName          :: Ident
  , indPropositional :: Bool
  , indKElim         :: AllowedElims
  , indTypeVars      :: WhenTyped [TypeVarInfo] t
  , indCtors         :: [ConstructorBody t]
  , indProjs         :: [ProjectionBody t]
  }

-- | Declaration of mutually defined inductive types
data MutualInductiveBody t = MutualInductive
  { indFinite :: RecursivityKind
  , indPars   :: Int
  , indBodies :: [OneInductiveBody t]
  }

-- | Definition of a constant in the environment
data ConstantBody t = ConstantBody
  { cstType :: WhenTyped LBox.Type t
  , cstBody :: Maybe Term
  }

-- | Global declarations.
data GlobalDecl :: Typing -> Type where
  ConstantDecl  :: ConstantBody t                   -> GlobalDecl t
  InductiveDecl :: MutualInductiveBody t            -> GlobalDecl t
  TypeAliasDecl :: Maybe ([TypeVarInfo], LBox.Type) -> GlobalDecl Typed

-- | Global environment.
newtype GlobalEnv t = GlobalEnv [(KerName, GlobalDecl t)]

-- | Generated module
data CoqModule t = CoqModule
  { coqEnv      :: GlobalEnv t
  , coqPrograms :: [KerName]
  }


-- pretty-printing
----------------------------

instance Pretty (ConstructorBody t) where
  pretty Constructor{..} = pretty cstrName <+> parens (pretty cstrArgs <+> "arg(s)")

instance Pretty (OneInductiveBody t) where
  pretty OneInductive{..} = vcat
    [ pretty indName
    , "constructors:" <+> pretty indCtors
    ]

instance Pretty (GlobalDecl t) where
  pretty = \case
    ConstantDecl ConstantBody{..} ->
      hang "constant:" 2 $ pretty cstBody

    InductiveDecl MutualInductive{..} ->
      hang "mutual inductive(s):" 2 $ 
        vsep $ map pretty indBodies

instance Pretty (GlobalEnv t) where
  pretty (GlobalEnv env) =
    vsep $ flip map (reverse env) \(kn, d) ->
      hang (pretty kn <> ":") 2 (pretty d)

instance Pretty (CoqModule t) where
  pretty CoqModule{..} = pretty coqEnv
