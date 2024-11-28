module LambdaBox where

type Ident = String
type KName = String
type Inductive = String

data Name = Anon | Named Ident
  deriving (Eq, Show)

data Def = Def Name Term Int
  deriving (Eq, Show)

data Term
  = Box
  | Var Int
  | Lam Name Term
  | App Term Term
  | Const KName
  | Ctor Inductive Int
  | Case Inductive Int Term [(Int, Term)]
  | Fix [Def] Int
  deriving (Eq, Show)
