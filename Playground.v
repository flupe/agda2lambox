From MetaCoq.Template      Require Import Loader Ast.
From MetaCoq.ErasurePlugin Require Import Erasure Loader.
From MetaCoq.Utils         Require Import utils.
From Coq Require Import ZArith List String Nat.

Import ListNotations.

Check run_erase_program.

Locate run_erase_program.

Program Definition cic_to_box (p : program) :=
  run_erase_program default_erasure_config p _.
Next Obligation.
  split.
  now eapply assume_that_we_only_erase_on_welltyped_programs.
  cbv [PCUICWeakeningEnvSN.normalizationInAdjustUniversesIn].
  pose proof @PCUICSN.normalization.
  split; typeclasses eauto.
Qed.

Definition not (x : bool) := 
  match x with 
    | true => true
    | false => false
  end.

Fixpoint double (n : nat) :=
  match n with 
    | 0   => 0
    | S n => S (S (double n))
  end.

Fixpoint even (n : nat) :=
  match n with 
    | 0 => true
    | S n => odd n
  end
with odd (n : nat)  :=
  match n with 
    | 0 => false
    | S n => even n
  end.

Set Primitive Projections.
Set Printing Projections.

Record Prod (A B : Type) : Type := Pair
  { fst : A
  ; snd : B
  }.

Arguments Pair {_ _} _ _.
Arguments fst {_ _} _.
Arguments snd {_ _} _.

Definition pair : Prod bool bool := Pair true false.

Definition prog  := pair.(fst).

MetaCoq Quote Recursively Definition ex1 := fst.
Check ex1.
Print ex1.
Eval vm_compute in cic_to_box ex1.
