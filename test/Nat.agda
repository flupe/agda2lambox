open import Agda.Builtin.Nat

one : Nat
one = suc zero

{-
add : Nat → Nat → Nat
add zero y = y
add (suc x) y = suc (add x y)

incr : Nat → Nat
incr = suc

deux trois : Nat
deux  = suc (suc zero)
trois = suc (suc (suc zero))

prog : Nat
prog = add trois deux
{-# COMPILE AGDA2LAMBOX prog #-}
-}
