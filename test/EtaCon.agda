module EtaCon where

open import Agda.Builtin.Nat
open import Agda.Builtin.List

addone : Nat → Nat
addone = suc

cons : {A : Set} → A → List A → List A
cons = _∷_
