From MetaCoq.Template Require Import Loader.
From MetaCoq.ErasurePlugin Require Import Erasure Loader.
From MetaCoq.Utils Require Import utils.
From MetaCoq.Template Require Import Ast.
From MetaCoq.Erasure.Typed Require Import ResultMonad.
From MetaCoq.Erasure.Typed Require Import Optimize.
From MetaCoq.Erasure.Typed Require Import Extraction.
From Coq Require Import ZArith.
From Coq Require Import List.
From Coq Require Import String.
From Coq Require Import Nat.

Import MCMonadNotation.
Import ListNotations.

Local Open Scope string.
Local Notation "'bs_to_s' s" := (bytestring.String.to_string s) (at level 200).
Local Notation "'s_to_bs' s" := (bytestring.String.of_string s) (at level 200).


Program Definition cic_to_box p :=
  run_erase_program default_erasure_config ([], p) _.
Next Obligation.
  split. easy.
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

Definition prog  := double.

MetaCoq Quote Recursively Definition ex1 := prog.
Eval vm_compute in cic_to_box ex1.
