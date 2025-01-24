open import Agda.Builtin.Nat

-- we have to redefine addition on builtin nats
-- because the builtin _+_ doesn't compile yet
add : Nat → Nat → Nat
add zero y = y
add (suc x) y = suc (add x y)

variable n : Nat

data Type : Set where
  ℕ   : Type
  _⇒_ : Type → Type → Type

variable α β : Type

infixl 7 _▷_
data Ctx : Nat → Set where
  []  : Ctx zero
  _▷_ : Ctx n → Type → Ctx (suc n)

variable Γ : Ctx n

infix 6 _∋_
data _∋_ : Ctx n → Type → Set where
  here  :          Γ ▷ α ∋ α
  there : Γ ∋ α → Γ ▷ β ∋ α

data Tm (Γ : Ctx n) : Type → Set where
  var : Γ ∋ α → Tm Γ α
  lam : Tm (Γ ▷ α) β → Tm Γ (α ⇒ β)
  app : Tm Γ (α ⇒ β) → Tm Γ α → Tm Γ β

  lit : Nat → Tm Γ ℕ
  _`+_ : Tm Γ ℕ → Tm Γ ℕ → Tm Γ ℕ

⟦_⟧ : Type → Set
⟦ ℕ     ⟧ = Nat
⟦ α ⇒ β ⟧ = ⟦ α ⟧ → ⟦ β ⟧

data Env : Ctx n → Set where
  []  : Env []
  _▷_ : Env Γ → ⟦ α ⟧ → Env (Γ ▷ α)

lookupEnv : Env Γ → Γ ∋ α → ⟦ α ⟧
lookupEnv (env ▷ x) here = x
lookupEnv (env ▷ _) (there k) = lookupEnv env k

eval : Env Γ → Tm Γ α → ⟦ α ⟧
eval env (var k)   = lookupEnv env k
eval env (lam u)   = λ x → eval (env ▷ x) u
eval env (app u v) = eval env u (eval env v)
eval env (lit x)   = x
eval env (u `+ v)   = add (eval env u) (eval env v)

test : Nat
test = eval [] (app (lam (lit 1 `+ var here)) (lit 1))
{-# COMPILE AGDA2LAMBOX test #-}
