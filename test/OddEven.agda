data Bool : Set where true false : Bool

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat

odd even : Nat -> Bool

odd zero = false
odd (succ n) = even n

even zero = true
even (succ n) = odd n
