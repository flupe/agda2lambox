module LambdaBox where
import GHC.Stack.CCS (whereFrom)

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
    

data Term
  = Box
  | BVar Int
  | FVar Ident
  | Lam Name Term
  | Let Name Term Term
  | App Term Term
  | Const KName
  | Ctor Inductive Int
  | Case Inductive Int Term [(Int, Term)]
  | Fix [Def] Int
  deriving (Eq, Show)
