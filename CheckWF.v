From Coq Require Import List.
From Coq Require Import Logic.Decidable.
From Coq Require Import Program.Equality.
From MetaCoq.Common Require Import BasicAst Kernames Universes EnvMap.
From MetaCoq.Erasure Require Import EAst EWellformed.
From MetaCoq.Utils Require Import bytestring.

Import ListNotations.
Import EnvMap.


(* Freshness of kername in the environment is decidable *)
Fixpoint dec_fresh_global
  (k     : kername)
  (decls : global_declarations)
  : decidable (fresh_global k decls).
  destruct decls.
(*
  - left.
    apply Forall_nil.
  - pose proof (dec_fresh_global k decls).
    remember (Kername.eq_dec (fst p) k) as peqk.
    destruct H.
    + left. refine (Forall_cons p _ H).
*)
Admitted.

(* Well-formedness of global declarations is decidable *)
Fixpoint dec_wf_glob
  {efl   : EEnvFlags}
  (decls : global_declarations)
  : decidable (wf_glob decls).
  destruct decls.
  - left.
    apply wf_glob_nil.
  - destruct (dec_wf_glob efl decls).
    +  destruct p.
       remember (wf_global_decl decls g)   as wf_g.
       remember (dec_fresh_global k decls) as fresh_k.
       destruct wf_g.
       destruct fresh_k.
       left.
       apply (wf_glob_cons k g decls H).
       rewrite <- Heqwf_g.
       auto.
       auto.
       right.
       intro.
       dependent destruction H0.
       auto.
       right.
       intro.
       dependent destruction H0.
       rewrite <- Heqwf_g in H1.
       unfold is_true in H1.
       discriminate H1.
    + right.
      intro wf_glob_decls.
      dependent destruction wf_glob_decls.
      contradiction.
Defined.

