{-# LANGUAGE FlexibleInstances #-}
module Lambox2Coq where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda2Lambox.Convert.Class(type (~>)(..), (:~>))
import LambdaBox(Def(..), Name(..), Term(..))

-- Converting the Haskell LambdaBox AST to MetaCoq's LambdaBox AST

type Coq = String

name2Coq :: Name -> Coq
name2Coq Anon = "nAnon"
name2Coq (Named i) = "(nNamed " <> show i <> ")"

def2Coq :: Def -> Coq
def2Coq (Def name term rarg) =
  "{| name := " <> name2Coq name <>
  "; dbody := " <> term2Coq term <>
  "; rarg := " <> show rarg <>
  "|}"

term2Coq :: Term -> Coq
term2Coq =  \case
  Box -> term "tBox" []
  BVar n -> term "tRel" [show n] -- TODO: Not sure if tRel is the right constructor
  FVar i -> term "tVar" [show i]
  Lam na e -> term "tLambda" [name2Coq na, term2Coq e]
  Let na b e -> term "tLetIn" [name2Coq na, term2Coq b, term2Coq e]
  App f e -> term "tApp" [term2Coq f, term2Coq e]
  Const k -> term "tConst" [show k]
  Ctor ind idx -> term "tConstruct" [show ind, show idx]
  Case ind n c brs ->
    term "tCase"
    [ pair (show ind) (show n)
    , term2Coq c
    , list $ uncurry pair . bimap show term2Coq <$> brs -- TODO: `brs` has a different type in MetaCoq
    ]
  Fix defs idx -> term "tFix" [list $ def2Coq <$> defs, show idx]

  where
    term :: String -> [String] -> String
    term ctor [] = ctor
    term ctor args = "(" <> ctor <> " " <> unwords args <> ")"

    pair :: String -> String -> String
    pair a b = "(" <> a <> ", " <> ")"

    list :: [String] -> String
    list ss = "[" <> intercalate "; " ss <> "]"

instance Name ~> Coq where
  go = pure . name2Coq

instance Def ~> Coq where
  go = pure . def2Coq

instance Term ~> Coq where
  go = pure . term2Coq
