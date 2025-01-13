data Nat : Set where
  zero : Nat
  succ : Nat → Nat

one : Nat
one = succ zero

add : Nat → Nat → Nat
add zero y = y
add (succ x) y = succ (add x y)

deux trois : Nat
deux  = succ (succ zero)
trois = succ (succ (succ zero))

prog : Nat
prog = add trois deux
{-# COMPILE AGDA2LAMBOX prog #-}
