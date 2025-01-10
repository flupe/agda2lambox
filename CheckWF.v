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

(* TODO: Freshness of kername in the environment is decidable *)
Definition dec_fresh_global
  (k     : kername)
  (decls : global_declarations)
  : decidable (fresh_global k decls).
Admitted.

Equations dec_wf_glob {efl : EEnvFlags} (decls : global_declarations)
  : decidable (wf_glob decls) :=
dec_wf_glob [] := or_introl wf_glob_nil;
dec_wf_glob ((k,d)::ds) with dec_wf_glob ds := {
  | or_introl wf_ds with inspect (wf_global_decl ds d), dec_fresh_global k ds := {
      | true  eqn: wf_d | or_introl fresh_k  := or_introl (wf_glob_cons k d ds wf_ds wf_d fresh_k);
      | true  eqn: wf_d | or_intror rotten_k := or_intror _;
      | false eqn: wf_d | _                  := or_intror _;
    };
  | or_intror p := or_intror _
}.
Next Obligation.
  intros dec_wf_glob efl k d ds fresh_k not_wf_d _ _ wf_decls.
  dependent elimination wf_decls.
  simpl in not_wf_d.
  rewrite not_wf_d in i.
  discriminate i.
Qed.
Next Obligation.
  intros dec_wf_glob efl k d ds rotten_k _ _ wf_decls.
  dependent elimination wf_decls.
  apply (rotten_k f).
Qed.
Next Obligation.
  intros dec_wf_glob efl k d ds rotten_k _ _ _ wf_decls.
  dependent elimination wf_decls.
  apply (rotten_k f).
Qed.
Next Obligation.
  intros dec_wf_glob efl k d ds  not_wf_ds wf_decls.
  dependent elimination wf_decls.
  apply (not_wf_ds w).
Defined.
