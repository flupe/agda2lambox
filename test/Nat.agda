data Nat : Set where
  zero : Nat
  succ : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

add : Nat → Nat → Nat
add zero y = y
add (succ x) y = succ (add x y)

prog : Nat
prog = add 3 2
{-# COMPILE AGDA2LAMBOX prog #-}

-- 5
res : Nat
res = 5
