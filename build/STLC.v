From Coq             Require Import List.
From MetaCoq.Common  Require Import BasicAst Kernames Universes.
From MetaCoq.Utils   Require Import bytestring.
From MetaCoq.Erasure Require Import EAst.
From Agda2Lambox     Require Import CheckWF Eval.
Import ListNotations.

Definition env : global_declarations :=
  [((MPfile ["STLC"%bs], "test"%bs),
    ConstantDecl
      {| cst_body :=
           Some
             (tApp
                (tApp
                   (tApp
                      (tApp
                         (tApp
                            (tConst (MPfile ["STLC"%bs], "eval"%bs))
                            (tConstruct
                               {| inductive_mind :=
                                    (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                  inductive_ind := 0 |}
                               0 []))
                         (tConstruct
                            {| inductive_mind := (MPfile ["STLC"%bs], "Ctx"%bs);
                               inductive_ind := 0 |}
                            0 []))
                      (tConstruct
                         {| inductive_mind := (MPfile ["STLC"%bs], "Type"%bs);
                            inductive_ind := 0 |}
                         0 []))
                   (tConstruct
                      {| inductive_mind := (MPfile ["STLC"%bs], "Env"%bs);
                         inductive_ind := 0 |}
                      0 []))
                (tConstruct
                   {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                      inductive_ind := 0 |}
                   2
                   [tConstruct
                      {| inductive_mind := (MPfile ["STLC"%bs], "Type"%bs);
                         inductive_ind := 0 |}
                      0 [];
                    tBox;
                    tConstruct
                      {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                         inductive_ind := 0 |}
                      1
                      [tBox; tBox;
                       tConstruct
                         {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                            inductive_ind := 0 |}
                         4
                         [tConstruct
                            {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                               inductive_ind := 0 |}
                            3
                            [tConstruct
                               {| inductive_mind :=
                                    (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                  inductive_ind := 0 |}
                               1
                               [tConstruct
                                  {| inductive_mind :=
                                       (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                     inductive_ind := 0 |}
                                  0 []]];
                          tConstruct
                            {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                               inductive_ind := 0 |}
                            0
                            [tBox;
                             tConstruct
                               {| inductive_mind := (MPfile ["STLC"%bs], "_\8715_"%bs);
                                  inductive_ind := 0 |}
                               0 [tBox; tBox; tBox]]]];
                    tConstruct
                      {| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                         inductive_ind := 0 |}
                      3
                      [tConstruct
                         {| inductive_mind :=
                              (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                            inductive_ind := 0 |}
                         1
                         [tConstruct
                            {| inductive_mind :=
                                 (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                               inductive_ind := 0 |}
                            0 []]]])) |});
   ((MPfile ["STLC"%bs], "eval"%bs),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLC.eval"%bs;
                    dbody :=
                      tLambda
                        nAnon
                        (tLambda
                           nAnon
                           (tLambda
                              nAnon
                              (tLambda
                                 nAnon
                                 (tLambda
                                    nAnon
                                    (tLetIn
                                       nAnon tBox
                                       (tCase
                                          ({| inductive_mind := (MPfile ["STLC"%bs], "Tm"%bs);
                                              inductive_ind := 0 |},
                                           0)
                                          (tRel 1)
                                          [([nAnon; nAnon],
                                            tApp
                                              (tApp
                                                 (tApp
                                                    (tApp
                                                       (tApp
                                                          (tConst
                                                             (MPfile ["STLC"%bs], "lookupEnv"%bs))
                                                          (tRel 7))
                                                       (tRel 6))
                                                    tBox)
                                                 (tRel 4))
                                              (tRel 0));
                                           ([nAnon; nAnon; nAnon],
                                            tLetIn
                                              nAnon tBox
                                              (tCase
                                                 ({| inductive_mind :=
                                                       (MPfile ["STLC"%bs], "Type"%bs);
                                                     inductive_ind := 0 |},
                                                  0)
                                                 (tRel 7)
                                                 [([], tRel 0);
                                                  ([nAnon; nAnon],
                                                   tLambda
                                                     nAnon
                                                     (tApp
                                                        (tApp
                                                           (tApp
                                                              (tApp
                                                                 (tApp
                                                                    (tRel 13)
                                                                    (tConstruct
                                                                       {| inductive_mind :=
                                                                            (MPfile
                                                                               ["Agda"%bs;
                                                                                "Builtin"%bs;
                                                                                "Nat"%bs],
                                                                             "Nat"%bs);
                                                                          inductive_ind := 0 |}
                                                                       1 [tRel 12]))
                                                                 (tConstruct
                                                                    {| inductive_mind :=
                                                                         (MPfile ["STLC"%bs],
                                                                          "Ctx"%bs);
                                                                       inductive_ind := 0 |}
                                                                    1 [tBox; tRel 11; tRel 2]))
                                                              (tRel 1))
                                                           (tConstruct
                                                              {| inductive_mind :=
                                                                   (MPfile ["STLC"%bs], "Env"%bs);
                                                                 inductive_ind := 0 |}
                                                              1 [tBox; tBox; tBox; tRel 9; tRel 0]))
                                                        (tRel 4)))]));
                                           ([nAnon; nAnon; nAnon; nAnon],
                                            tApp
                                              (tApp
                                                 (tApp
                                                    (tApp
                                                       (tApp (tApp (tRel 10) (tRel 9)) (tRel 8))
                                                       (tConstruct
                                                          {| inductive_mind :=
                                                               (MPfile ["STLC"%bs], "Type"%bs);
                                                             inductive_ind := 0 |}
                                                          1 [tRel 3; tRel 7]))
                                                    (tRel 6))
                                                 (tRel 1))
                                              (tApp
                                                 (tApp
                                                    (tApp
                                                       (tApp (tApp (tRel 10) (tRel 9)) (tRel 8))
                                                       (tRel 3))
                                                    (tRel 6))
                                                 (tRel 0)));
                                           ([nAnon], tRel 0);
                                           ([nAnon; nAnon],
                                            tApp
                                              (tApp
                                                 (tConst (MPfile ["STLC"%bs], "add"%bs))
                                                 (tApp
                                                    (tApp
                                                       (tApp
                                                          (tApp (tApp (tRel 8) (tRel 7)) (tRel 6))
                                                          (tConstruct
                                                             {| inductive_mind :=
                                                                  (MPfile ["STLC"%bs], "Type"%bs);
                                                                inductive_ind := 0 |}
                                                             0 []))
                                                       (tRel 4))
                                                    (tRel 1)))
                                              (tApp
                                                 (tApp
                                                    (tApp
                                                       (tApp (tApp (tRel 8) (tRel 7)) (tRel 6))
                                                       (tConstruct
                                                          {| inductive_mind :=
                                                               (MPfile ["STLC"%bs], "Type"%bs);
                                                             inductive_ind := 0 |}
                                                          0 []))
                                                    (tRel 4))
                                                 (tRel 0)))]))))));
                    rarg := 0 |}]
                0) |});
   ((MPfile ["STLC"%bs], "lookupEnv"%bs),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLC.lookupEnv"%bs;
                    dbody :=
                      tLambda
                        nAnon
                        (tLambda
                           nAnon
                           (tLambda
                              nAnon
                              (tLambda
                                 nAnon
                                 (tLambda
                                    nAnon
                                    (tLetIn
                                       nAnon tBox
                                       (tCase
                                          ({| inductive_mind := (MPfile ["STLC"%bs], "Env"%bs);
                                              inductive_ind := 0 |},
                                           0)
                                          (tRel 2)
                                          [([], tRel 0);
                                           ([nAnon; nAnon; nAnon; nAnon; nAnon],
                                            tLetIn
                                              nAnon tBox
                                              (tCase
                                                 ({| inductive_mind :=
                                                       (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs],
                                                        "Nat"%bs);
                                                     inductive_ind := 0 |},
                                                  0)
                                                 (tRel 11)
                                                 [([], tRel 0);
                                                  ([nAnon],
                                                   tLetIn
                                                     nAnon tBox
                                                     (tCase
                                                        ({| inductive_mind :=
                                                              (MPfile ["STLC"%bs], "Ctx"%bs);
                                                            inductive_ind := 0 |},
                                                         0)
                                                        (tRel 12)
                                                        [([], tRel 0);
                                                         ([nAnon; nAnon; nAnon],
                                                          tLetIn
                                                            nAnon tBox
                                                            (tCase
                                                               ({| inductive_mind :=
                                                                     (MPfile ["STLC"%bs],
                                                                      "_\8715_"%bs);
                                                                   inductive_ind := 0 |},
                                                                0)
                                                               (tRel 13)
                                                               [([nAnon; nAnon; nAnon], tRel 10);
                                                                ([nAnon; nAnon; nAnon; nAnon;
                                                                  nAnon],
                                                                 tApp
                                                                   (tApp
                                                                      (tApp
                                                                         (tApp
                                                                            (tApp
                                                                               (tRel 23) (tRel 10))
                                                                            (tRel 7))
                                                                         tBox)
                                                                      (tRel 13))
                                                                   (tRel 0))]))]))]))]))))));
                    rarg := 0 |}]
                0) |});
   ((MPfile ["STLC"%bs], "Env"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Env"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "[]"%bs; cstr_nargs := 0 |};
                  {| cstr_name := "_\9655_"%bs; cstr_nargs := 5 |}];
               ind_projs := [] |}] |});
   ((MPfile ["STLC"%bs], "Tm"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Tm"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "var"%bs; cstr_nargs := 2 |};
                  {| cstr_name := "lam"%bs; cstr_nargs := 3 |};
                  {| cstr_name := "app"%bs; cstr_nargs := 4 |};
                  {| cstr_name := "lit"%bs; cstr_nargs := 1 |};
                  {| cstr_name := "_`+_"%bs; cstr_nargs := 2 |}];
               ind_projs := [] |}] |});
   ((MPfile ["STLC"%bs], "_\8715_"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "_\8715_"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "here"%bs; cstr_nargs := 3 |};
                  {| cstr_name := "there"%bs; cstr_nargs := 5 |}];
               ind_projs := [] |}] |});
   ((MPfile ["STLC"%bs], "Ctx"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Ctx"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "[]"%bs; cstr_nargs := 0 |};
                  {| cstr_name := "_\9655_"%bs; cstr_nargs := 3 |}];
               ind_projs := [] |}] |});
   ((MPfile ["STLC"%bs], "Type"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Type"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "\8469"%bs; cstr_nargs := 0 |};
                  {| cstr_name := "_\8658_"%bs; cstr_nargs := 2 |}];
               ind_projs := [] |}] |});
   ((MPfile ["STLC"%bs], "add"%bs),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLC.add"%bs;
                    dbody :=
                      tLambda
                        nAnon
                        (tLambda
                           nAnon
                           (tLetIn
                              nAnon tBox
                              (tCase
                                 ({| inductive_mind :=
                                       (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                     inductive_ind := 0 |},
                                  0)
                                 (tRel 2)
                                 [([], tRel 1);
                                  ([nAnon],
                                   tConstruct
                                     {| inductive_mind :=
                                          (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                        inductive_ind := 0 |}
                                     1 [tApp (tApp (tRel 4) (tRel 0)) (tRel 2)])])));
                    rarg := 0 |}]
                0) |});
   ((MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Nat"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [{| cstr_name := "zero"%bs; cstr_nargs := 0 |};
                  {| cstr_name := "suc"%bs; cstr_nargs := 1 |}];
               ind_projs := [] |}] |})].

Compute @check_wf_glob eflags env.

Definition prog1 : program :=
  (env, tConst (MPfile ["STLC"%bs], "test"%bs)).

Compute eval_program prog1.
