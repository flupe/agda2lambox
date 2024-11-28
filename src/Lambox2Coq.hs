{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module Lambox2Coq where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda2Lambox.Convert.Class(type (~>)(..), (:~>))
import LambdaBox(Def(..), Term(..))

type Coq = String

def2Coq :: Def -> Coq
def2Coq (Def name term rarg) =
  "{| name := " <> show name <>
  "; dbody := " <> term2Coq term <>
  "; rarg := " <> show rarg <>
  "|}"

term2Coq :: Term -> Coq
term2Coq =  \case
  Box -> term "tBox" []
  BVar n -> term "tRel" [show n] -- TODO: Not sure if tRel is the right constructor
  FVar x -> term "tVar" [show x]
  Lam x e -> term "tLambda" [show x, term2Coq e]
  Let x b e -> term "tLetIn" [show x, term2Coq b, term2Coq e]
  App f a -> term "tApp" [term2Coq f, term2Coq a]
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

instance Def ~> Coq where
  go :: Def :~> Coq
  go = pure . def2Coq

instance Term ~> Coq where
  go :: Term :~> Coq
  go = pure . term2Coq
