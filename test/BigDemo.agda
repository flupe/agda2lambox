module _ where

-- - decision procedure (barebones)
module BAREBONES where
  open import Agda.Primitive using (Level)
  open import Agda.Builtin.Bool using (Bool; true; false)
  open import Agda.Builtin.Nat using (Nat; zero; suc)
  open import Agda.Builtin.Equality using (_≡_; refl)

  private variable
    ℓ : Level
    A B : Set ℓ
    x y : Nat

  data ⊥ : Set where

  ¬_ : Set → Set
  ¬ A = A → ⊥

  data Dec (A : Set) : Set where
    yes : A → Dec A
    no  : ¬ A → Dec A

  isYes : Dec A → Bool
  isYes = λ where
    (yes _) → true
    (no  _) → false

  record HasDec (A : Set) : Set where
    field dec : Dec A
  open HasDec ⦃...⦄

  cong : ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y
  cong _ refl = refl

  instance
    HasDecEq-Nat : HasDec (x ≡ y)
    HasDecEq-Nat {x}{y} .dec with x | y
    ... | zero | zero = yes refl
    ... | zero | suc n = no λ ()
    ... | suc m | zero = no λ ()
    ... | suc m | suc n with dec
    ... | yes eq  = yes (cong suc eq)
    ... | no  m≢n = no λ where refl → m≢n refl

  test : Bool
  test = isYes (dec {A = 42 ≡ 42})
  -- {-# COMPILE AGDA2LAMBOX test #-}

-- - decision procedure (importing stdlib)
module STDLIB where
  open import Relation.Nullary using (isYes)

  open import Data.Bool using (Bool; true; false)
  open import Data.Bool.Properties using (_≟_)
  test : Bool
  test = isYes (true ≟ false)
  -- {-# COMPILE AGDA2LAMBOX test #-}

  -- open import Data.Bool using (Bool)
  -- open import Data.Nat.Properties using (_≟_)
  -- test : Bool
  -- test = isYes (42 ≟ 42)
  -- {-# COMPILE AGDA2LAMBOX test #-}

-- - sort
module SORT where
  open import Data.List.Base using (List; _∷_; [])

  open import Data.Bool.Properties using (≤-decTotalOrder)
  open import Data.Bool using (Bool; true; false)
  open import Data.List.Sort ≤-decTotalOrder using (sort)
  test : List Bool
  test = sort (true ∷ false ∷ true ∷ false ∷ [])
  {-# COMPILE AGDA2LAMBOX test #-}

  -- open import Data.Nat.Properties using (≤-decTotalOrder)
  -- open import Data.Bool using (Bool; true; false)
  -- open import Data.List.Base using (List; _∷_; [])
  -- open import Data.List.Sort ≤-decTotalOrder using (sort)
  -- test : List Bool
  -- test = sort (42 ∷ 0 ∷ 5 ∷ 3 ∷ [])
  -- {-# COMPILE AGDA2LAMBOX test #-}
