From Coq             Require Import List Logic.Decidable.
From MetaCoq.Common  Require Import BasicAst Kernames Universes EnvMap.
From MetaCoq.Erasure Require Import EAst EWellformed.
From MetaCoq.Utils   Require Import bytestring.
From Equations       Require Import Equations.

Import ListNotations.
Import EnvMap.

#[global] Obligation Tactic := idtac.
#[global] Set Equations Transparent.
#[global] Set Equations With UIP.

Definition inspect {A} (a : A) : {b | a = b} := exist _ a eq_refl.
Notation "x 'eqn:' p" := (exist _ x p) (only parsing, at level 20).

Print Forall.
Print fresh_global.

#[local] Obligation Tactic :=
          try intro wf_decls;
          try dependent elimination wf_decls;
          try auto.

Equations? dec_fresh_global (k : kername) (decls : global_declarations) : decidable (fresh_global k decls) :=
dec_fresh_global k [] := or_introl (Forall_nil _);
dec_fresh_global k ((k', _)::ds) with dec_fresh_global k ds := {
  | or_intror rotten_k_ds := or_intror _;
  | or_introl fresh_k_ds  with eq_dec k' k := {
     | left  k'eqk  := or_intror _;
     | right k'neqk := or_introl _
  };
}.
- apply Forall_cons; auto.
Defined.

Equations? dec_wf_glob {efl : EEnvFlags} (decls : global_declarations) : decidable (wf_glob decls) :=
dec_wf_glob [] := or_introl wf_glob_nil;
dec_wf_glob ((k, d)::ds) with dec_wf_glob ds := {
  | or_introl wf_ds with inspect (wf_global_decl ds d), dec_fresh_global k ds := {
      | true  eqn:     wf_d | or_introl fresh_k  := or_introl _;
      | true  eqn:     wf_d | or_intror rotten_k := or_intror _;
      | false eqn: not_wf_d | _                  := or_intror _;
    };
  | or_intror not_wf_ds := or_intror _
}.
- apply wf_glob_cons; auto.
- simpl in not_wf_d.
  rewrite not_wf_d in i.
  auto.
Defined.
