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
