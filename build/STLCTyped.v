From Coq             Require Import List.
From MetaCoq.Common  Require Import BasicAst Kernames Universes.
From MetaCoq.Utils   Require Import bytestring.
From MetaCoq.Erasure Require Import EAst ExAst.
From Agda2Lambox     Require Import CheckWF Eval.
Import ListNotations.

Definition env : global_env :=
  [(((MPfile ["STLCTyped"%bs], "test"%bs), true),
    ConstantDecl
      {| cst_body :=
           Some
             (tApp
                (tApp
                   (tApp
                      (tApp
                         (tApp
                            (tConst (MPfile ["STLCTyped"%bs], "eval"%bs))
                            (tConstruct
                               {| inductive_mind :=
                                    (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs);
                                  inductive_ind := 0 |}
                               0 []))
                         (tConstruct
                            {| inductive_mind := (MPfile ["STLCTyped"%bs], "Ctx"%bs);
                               inductive_ind := 0 |}
                            0 []))
                      (tConstruct
                         {| inductive_mind := (MPfile ["STLCTyped"%bs], "Type"%bs);
                            inductive_ind := 0 |}
                         0 []))
                   (tConstruct
                      {| inductive_mind := (MPfile ["STLCTyped"%bs], "Env"%bs);
                         inductive_ind := 0 |}
                      0 []))
                (tConstruct
                   {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
                      inductive_ind := 0 |}
                   2
                   [tConstruct
                      {| inductive_mind := (MPfile ["STLCTyped"%bs], "Type"%bs);
                         inductive_ind := 0 |}
                      0 [];
                    tBox;
                    tConstruct
                      {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
                         inductive_ind := 0 |}
                      1
                      [tBox; tBox;
                       tConstruct
                         {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
                            inductive_ind := 0 |}
                         4
                         [tConstruct
                            {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
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
                            {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
                               inductive_ind := 0 |}
                            0
                            [tBox;
                             tConstruct
                               {| inductive_mind := (MPfile ["STLCTyped"%bs], "_\8715_"%bs);
                                  inductive_ind := 0 |}
                               0 [tBox; tBox; tBox]]]];
                    tConstruct
                      {| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
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
                            0 []]]]));
         cst_type :=
           ([],
            TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs)) |});
   (((MPfile ["STLCTyped"%bs], "eval"%bs), true),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLCTyped.eval"%bs;
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
                                          ({| inductive_mind := (MPfile ["STLCTyped"%bs], "Tm"%bs);
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
                                                             (MPfile ["STLCTyped"%bs],
                                                              "lookupEnv"%bs))
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
                                                       (MPfile ["STLCTyped"%bs], "Type"%bs);
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
                                                                         (MPfile ["STLCTyped"%bs],
                                                                          "Ctx"%bs);
                                                                       inductive_ind := 0 |}
                                                                    1 [tBox; tRel 11; tRel 2]))
                                                              (tRel 1))
                                                           (tConstruct
                                                              {| inductive_mind :=
                                                                   (MPfile ["STLCTyped"%bs],
                                                                    "Env"%bs);
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
                                                               (MPfile ["STLCTyped"%bs], "Type"%bs);
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
                                                 (tConst (MPfile ["STLCTyped"%bs], "add"%bs))
                                                 (tApp
                                                    (tApp
                                                       (tApp
                                                          (tApp (tApp (tRel 8) (tRel 7)) (tRel 6))
                                                          (tConstruct
                                                             {| inductive_mind :=
                                                                  (MPfile ["STLCTyped"%bs],
                                                                   "Type"%bs);
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
                                                               (MPfile ["STLCTyped"%bs], "Type"%bs);
                                                             inductive_ind := 0 |}
                                                          0 []))
                                                    (tRel 4))
                                                 (tRel 0)))]))))));
                    rarg := 0 |}]
                0);
         cst_type :=
           ([],
            TArr
              (TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))
              (TArr
                 (TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs))
                 (TArr
                    (TConst (MPfile ["STLCTyped"%bs], "Type"%bs))
                    (TArr
                       (TConst (MPfile ["STLCTyped"%bs], "Env"%bs))
                       (TArr
                          (TApp (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) TAny) TAny)
                          TAny))))) |});
   (((MPfile ["STLCTyped"%bs], "lookupEnv"%bs), true),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLCTyped.lookupEnv"%bs;
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
                                          ({| inductive_mind := (MPfile ["STLCTyped"%bs], "Env"%bs);
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
                                                              (MPfile ["STLCTyped"%bs], "Ctx"%bs);
                                                            inductive_ind := 0 |},
                                                         0)
                                                        (tRel 12)
                                                        [([], tRel 0);
                                                         ([nAnon; nAnon; nAnon],
                                                          tLetIn
                                                            nAnon tBox
                                                            (tCase
                                                               ({| inductive_mind :=
                                                                     (MPfile ["STLCTyped"%bs],
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
                0);
         cst_type :=
           ([],
            TArr
              (TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))
              (TArr
                 (TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs))
                 (TArr
                    (TConst (MPfile ["STLCTyped"%bs], "Type"%bs))
                    (TArr
                       (TConst (MPfile ["STLCTyped"%bs], "Env"%bs))
                       (TArr
                          (TConst (MPfile ["STLCTyped"%bs], "_\8715_"%bs)) TAny))))) |});
   (((MPfile ["STLCTyped"%bs], "Env"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Env"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("[]"%bs, []), 0);
                  (("_\9655_"%bs,
                    [(nNamed "\915.n"%bs,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs));
                     (nNamed "\915"%bs, TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs));
                     (nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "Env"%bs));
                     (nAnon, TAny)]),
                   5)];
               ind_projs := []; ind_type_vars := [] |}] |});
   (((MPfile ["STLCTyped"%bs], "\10214_\10215"%bs), true),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLCTyped.\10214_\10215"%bs; dbody := tBox;
                    rarg := 0 |}]
                0);
         cst_type :=
           ([], TArr (TConst (MPfile ["STLCTyped"%bs], "Type"%bs)) TBox) |});
   (((MPfile ["STLCTyped"%bs], "Tm"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Tm"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("var"%bs,
                    [(nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "_\8715_"%bs))]),
                   2);
                  (("lam"%bs,
                    [(nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nNamed "\946"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon,
                      TApp
                        (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) TAny) TAny)]),
                   3);
                  (("app"%bs,
                    [(nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nNamed "\946"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon,
                      TApp
                        (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) (TVar 0))
                        (TVar 1));
                     (nAnon,
                      TApp
                        (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) (TVar 0))
                        (TVar 1))]),
                   4);
                  (("lit"%bs,
                    [(nAnon,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))]),
                   1);
                  (("_`+_"%bs,
                    [(nAnon,
                      TApp
                        (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) (TVar 0))
                        (TVar 1));
                     (nAnon,
                      TApp
                        (TApp (TConst (MPfile ["STLCTyped"%bs], "Tm"%bs)) (TVar 0))
                        (TVar 1))]),
                   2)];
               ind_projs := [];
               ind_type_vars :=
                 [{| tvar_name := nNamed "n"%bs; tvar_is_logical := false;
                     tvar_is_arity := false; tvar_is_sort := false |};
                  {| tvar_name := nNamed "\915"%bs; tvar_is_logical := false;
                     tvar_is_arity := false; tvar_is_sort := false |}] |}] |});
   (((MPfile ["STLCTyped"%bs], "_\8715_"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "_\8715_"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("here"%bs,
                    [(nNamed "\915.n"%bs,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs));
                     (nNamed "\915"%bs, TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs));
                     (nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs))]),
                   3);
                  (("there"%bs,
                    [(nNamed "\915.n"%bs,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs));
                     (nNamed "\915"%bs, TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs));
                     (nNamed "\945"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nNamed "\946"%bs, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "_\8715_"%bs))]),
                   5)];
               ind_projs := []; ind_type_vars := [] |}] |});
   (((MPfile ["STLCTyped"%bs], "Ctx"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Ctx"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("[]"%bs, []), 0);
                  (("_\9655_"%bs,
                    [(nNamed "n"%bs,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "Ctx"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "Type"%bs))]),
                   3)];
               ind_projs := []; ind_type_vars := [] |}] |});
   (((MPfile ["STLCTyped"%bs], "Type"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Type"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("\8469"%bs, []), 0);
                  (("_\8658_"%bs,
                    [(nAnon, TConst (MPfile ["STLCTyped"%bs], "Type"%bs));
                     (nAnon, TConst (MPfile ["STLCTyped"%bs], "Type"%bs))]),
                   2)];
               ind_projs := []; ind_type_vars := [] |}] |});
   (((MPfile ["STLCTyped"%bs], "add"%bs), true),
    ConstantDecl
      {| cst_body :=
           Some
             (tFix
                [{| dname := nNamed "STLCTyped.add"%bs;
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
                0);
         cst_type :=
           ([],
            TArr
              (TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))
              (TArr
                 (TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))
                 (TConst
                    (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs)))) |});
   (((MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs), true),
    InductiveDecl
      {| ind_finite := Finite; ind_npars := 0;
         ind_bodies :=
           [{| ind_name := "Nat"%bs; ind_propositional := false;
               ind_kelim := IntoAny;
               ind_ctors :=
                 [(("zero"%bs, []), 0);
                  (("suc"%bs,
                    [(nNamed "n"%bs,
                      TConst (MPfile ["Agda"%bs; "Builtin"%bs; "Nat"%bs], "Nat"%bs))]),
                   1)];
               ind_projs := []; ind_type_vars := [] |}] |})].
