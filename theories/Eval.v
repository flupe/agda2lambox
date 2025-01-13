(* This file provides utilises to evaluate lambda box programs *)

From Coq                   Require Import Nat.
From MetaCoq.Utils         Require Import utils.
From MetaCoq.Erasure       Require Import EAst.
From CertiCoq.Common       Require Import Common.
From CertiCoq.LambdaBoxMut Require Import compile term program wcbvEval.

(* convert a lambda box program to certicoq lambda box mut, and run it *)
Definition eval_program (p : EAst.program) : exception Term :=
  let prog := {| env  := LambdaBoxMut.compile.compile_ctx (fst p);
      main := compile (snd p)
  |} 
  in wcbvEval (env prog) (2 ^ 14) (main prog).