{-# LANGUAGE FlexibleInstances #-}
-- | Converting the Haskell LambdaBox AST to MetaCoq's LambdaBox AST
module Lambox2Coq where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda2Lambox.Convert.Class(type (~>)(..), (:~>))
import LambdaBox(Def(..), Name(..), Term(..), Inductive(..))

-- Helpers for generating Coq syntax

type Coq = String

ctor :: String -> [Coq] -> Coq
ctor ctor [] = ctor
ctor ctor args = "(" <> ctor <> " " <> unwords args <> ")"

pair :: Coq -> Coq -> Coq
pair a b = "(" <> a <> ", " <> b <> ")"

list :: [Coq] -> Coq
list ss = "[" <> intercalate "; " ss <> "]"

record :: [(String, Coq)] -> Coq
record fields = "{| " <> intercalate "; " (field <$> fields) <> " |}"
  where
    field (name, value) = name <> " := " <> value

-- Translating LambdaBox terms to Coq

name2Coq :: Name -> Coq
name2Coq Anon = ctor "nAnon" []
name2Coq (Named i) = ctor "nNamed" [show i]

def2Coq :: Def -> Coq
def2Coq (Def name term rarg) =
  record
  [ ("name", name2Coq name)
  , ("dbody", term2Coq term)
  , ("rarg", show rarg)
  ]

ind2Coq :: Inductive -> Coq
ind2Coq (Inductive mind ind) = record
  [ ("inductive_mind", pair (ctor "MPfile" ["[]"]) (show mind))
  , ("inductive_ind" , show ind)
  ]

term2Coq :: Term -> Coq
term2Coq =  \case
  Box              -> ctor "tBox" []
  Rel n            -> ctor "tRel" [show n]
  Var i            -> ctor "tVar" [show i]
  Lam e            -> ctor "tLambda" [name2Coq Anon, term2Coq e]
  Let b e          -> ctor "tLetIn" [name2Coq Anon, term2Coq b, term2Coq e]
  App f e          -> ctor "tApp" [term2Coq f, term2Coq e]
  Const k          -> ctor "tConst" [show k]
  Ctor ind idx     -> ctor "tConstruct" [ind2Coq ind, show idx]
  Fix defs idx     -> ctor "tFix" [list $ def2Coq <$> defs, show idx]
  Case ind n c brs ->
    ctor "tCase"
    [ ind2Coq ind
    , term2Coq c
    , list $ uncurry pair . bimap show term2Coq <$> brs -- TODO: `brs` has a different type in MetaCoq
    ]

instance Name ~> Coq where
  go = pure . name2Coq

instance Def ~> Coq where
  go = pure . def2Coq

instance Term ~> Coq where
  go = pure . term2Coq
