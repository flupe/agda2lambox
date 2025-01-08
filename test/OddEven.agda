data Bool : Set where true false : Bool

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

double : Nat -> Nat
double zero = zero
double (succ n) = succ (succ (double n))

odd even : Nat -> Bool

odd zero = false
odd (succ n) = even n

even zero = true
even (succ n) = odd n
