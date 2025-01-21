open import Agda.Builtin.Nat
open import Agda.Builtin.List

xs : List Nat
xs = zero ∷ []

map : {A B : Set} -> (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

ys : List Nat
ys = map (2 +_) xs
{-# COMPILE AGDA2LAMBOX ys #-}
