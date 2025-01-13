data Nat : Set where
  zero : Nat
  succ : Nat → Nat

add : Nat -> Nat -> Nat
add zero y = y
add (succ x) y = succ (add x y)

infixr 7 _::_
data List (A : Set) : Set where
  []   : List A
  _::_ : A -> List A -> List A

xs : List Nat
xs = zero :: succ zero :: succ (succ zero) :: []

map : {A B : Set} -> (A -> B) -> List A -> List B
map f [] = []
map f (x :: xs) = f x :: map f xs

ys : List Nat
ys = map (\x -> add x x) xs
{-# COMPILE AGDA2LAMBOX ys #-}
