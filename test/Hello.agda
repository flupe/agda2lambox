open import Agda.Builtin.Nat
open import Agda.Primitive

infixr 7 _::_
data List {a : Level} (A : Set a) : Set a where
  []   : List A
  _::_ : A → List A → List A

String : Set
String = List Nat

hello : String
hello = 72 :: 101 :: 108 :: 108 :: 111 :: 33 :: []
{-# COMPILE AGDA2LAMBOX hello #-}
