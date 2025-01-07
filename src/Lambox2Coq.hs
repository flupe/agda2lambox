{-# LANGUAGE FlexibleInstances #-}
-- | Converting the Haskell LambdaBox AST to MetaCoq's LambdaBox AST
module Lambox2Coq where

import Data.Bifunctor(bimap)
import Data.List(intercalate)

import Agda2Lambox.Convert.Class(type (~>)(..), (:~>))
import LambdaBox

-- Helpers for generating Coq syntax

for = flip map

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

-- TODO(flupe): proper handling of kernames!
kername :: String -> Coq
kername ident = pair (ctor "MPfile" [list []]) (show ident)

-- Translating LambdaBox terms to Coq

name2Coq :: Name -> Coq
name2Coq Anon = ctor "nAnon" []
name2Coq (Named i) = ctor "nNamed" [show i]

def2Coq :: Def -> Coq
def2Coq (Def name term rarg) =
  record
  [ ("dname", name2Coq name)
  , ("dbody", term2Coq term)
  , ("rarg", show rarg)
  ]

ind2Coq :: Inductive -> Coq
ind2Coq (Inductive mind ind) = record
  [ ("inductive_mind", kername mind)
  , ("inductive_ind" , show ind)
  ]

dirpath2Coq :: DirPath -> Coq
dirpath2Coq = list . map show

modpath2Coq :: ModPath -> Coq
modpath2Coq = \case
  MPFile dp       -> ctor "MPfile" [dirpath2Coq dp]
  MPBound dp id i -> ctor "MPbound" [dirpath2Coq dp, show id, show i]
  MPDot mp id     -> ctor "MPdot" [modpath2Coq mp, show id]

kername2Coq :: KerName -> Coq
kername2Coq KerName{..} = pair (modpath2Coq kerModPath) (show kerName)

constant2Coq :: ConstantBody -> Coq
constant2Coq Nothing = ctor "None" []
constant2Coq (Just x) = ctor "Some" [term2Coq x]

decl2Coq :: GlobalDecl -> Coq
decl2Coq = \case
  ConstantDecl body   -> ctor "ConstantDecl" [record [("cst_body", constant2Coq body)]]
  InductiveDecl mbody -> error "I cannot do this rn"

decls2Coq :: [(KerName, GlobalDecl)] -> Coq
decls2Coq = list . map (uncurry pair . bimap kername2Coq decl2Coq)

term2Coq :: Term -> Coq
term2Coq =  \case
  LBox              -> ctor "tBox" []
  LRel n            -> ctor "tRel" [show n]
  LVar i            -> ctor "tVar" [show i]
  LLam e            -> ctor "tLambda" [name2Coq Anon, term2Coq e]
  LLet b e          -> ctor "tLetIn" [name2Coq Anon, term2Coq b, term2Coq e]
  LApp f e          -> ctor "tApp" [term2Coq f, term2Coq e]
  LConst k          -> ctor "tConst" [kername k]
  LCtor ind idx     -> ctor "tConstruct" [ind2Coq ind, show idx, list []]
  LFix defs idx     -> ctor "tFix" [list $ def2Coq <$> defs, show idx]
  LCase ind n c brs ->
    let cbrs :: [Coq] = for brs \(nargs, b) ->
          let binders = take nargs $ repeat Anon
          in pair (list $ map name2Coq binders) (term2Coq b)
    in ctor "tCase"
    [ pair (ind2Coq ind) (show n) -- TODO(flupe): figure out what this index should be
    , term2Coq c
    , list cbrs
    ]

instance Name ~> Coq where
  go = pure . name2Coq

instance Def ~> Coq where
  go = pure . def2Coq

instance Term ~> Coq where
  go = pure . term2Coq
