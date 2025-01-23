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
  , cstrTypes :: WhenTyped t [(Name, LBox.Type)]
  }

-- | Projection info in datatype declaration.
data ProjectionBody t = Projection
  { projName :: Ident
  , projType :: WhenTyped t LBox.Type
  }

data TypeVarInfo = TypeVarInfo
  { tvarName      :: Name
  , tvarIsLogical :: Bool
     -- ^ A parameter @t@ is *logical* if it is "a proposition when fully applied"
     --    i.e @t : Prop@ means  t is logical,
     --        @t a₁ a₂ : Prop@ means t is logical
  , tvarIsArity   :: Bool
     -- ^ a parameter @t@ is an arity if it is a type when fully applied.
     --     Consider @(T : Type → Type) (A : T Nat)@. @A@ is an arity, because @T : @
  , tvarIsSort    :: Bool
  }

-- | Inductive datatype declaration body
data OneInductiveBody t = OneInductive
  { indName          :: Ident
  , indPropositional :: Bool
  , indKElim         :: AllowedElims
  , indTypeVars      :: WhenTyped t [TypeVarInfo]
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
  { cstType :: WhenTyped t ([Name], LBox.Type)
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
      vsep
      [ hang "constant:" 2 $ pretty cstBody
      , foldMap (("type:" <+>) . pretty) cstType
      ]

    InductiveDecl MutualInductive{..} ->
      hang "mutual inductive(s):" 2 $ 
        vsep $ map pretty indBodies

instance Pretty (GlobalEnv t) where
  pretty (GlobalEnv env) =
    vsep $ flip map (reverse env) \(kn, d) ->
      hang (pretty kn <> ":") 2 (pretty d)

instance Pretty (CoqModule t) where
  pretty CoqModule{..} = pretty coqEnv
