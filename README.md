# agda2lambox

An [Agda] backend to generate [MetaCoq] λ□ (LambdaBox) programs
for further (verified) extraction to WASM or Rust.
The backend builds off Agda 2.7.0.1.

[Agda]: https://github.com/agda/agda
[MetaCoq]: https://metacoq.github.io/

## Setup

Compatible with Coq 8.19.0, MetaCoq 1.3.1 and CertiCoq 0.9.

```
opam pin add certicoq 0.9+8.19
coq_makefile -f _CoqProject -o CoqMakefile
make -f CoqMakefile
cabal run agda2lambox -- --out-dir build -itest test/Nat.agda
```

## Status

- Compilation to untyped λ□ programs:
  - Mutually-defined datatypes and record types.
  - Mutually-defined functions.
  - Importing modules.
    The backend transitively compiles all required definitions, and only those.
  - Nat literals.

- Compilation to typed λ□ programs:
  - Compile function types.
  - [ ] Type of constructors.
  - [ ] Type of projections.
  - [ ] Type aliases.
  - [ ] CertiCoq/MetaCoq type-checker on generated environments?

## TODO

- [ ] Better error-reporting
- [ ] Check support for Agda-specific edge cases
  - [x] Pattern-matching lambdas
  - [x] With-generated lambdas
  - [ ] Module applications
  - [ ] Projection-like
  - [ ] Check that treeless generates exhaustive cases
- [ ] Support primitives (ints and floats)

- [ ] Setup compilation to Wasm/Rust using Certicoq
- [ ] Setup proper testing infrastructure

## Icebox

Things that ought to be implemented, but not right now.

- [ ] Caching of compiled modules.

## References

- [The MetaCoq Project](https://github.com/MetaCoq/metacoq)
- [The CertiCoq Compilation pipeline](https://github.com/CertiCoq/certicoq/wiki/The-CertiCoq-pipeline)
- [CertiCoqWASM](https://github.com/womeier/certicoqwasm)
- [Pierre Letouzey's thesis introducing λ□](https://www.irif.fr/~letouzey/download/these_letouzey.pdf) (in French)
- [Verified Extraction from Coq to OCaml](https://github.com/yforster/coq-verified-extraction/)
  and its [accompanying paper](https://dl.acm.org/doi/10.1145/3656379)
- [Certified Erasure for Coq, in Coq](https://inria.hal.science/hal-04077552)
- [Extracting functional programs from Coq, in Coq](https://arxiv.org/pdf/2108.02995)
- [Lambdabox syntax and untyped environments](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/EAst.v) 
- [Lambdabox typed environments](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/ExAst.v) 
- [Coq Extraction Pipeline](https://gist.github.com/4ever2/991007b4418b0ba44f2ee7ed51147e19)
- [Extraction Example](https://gist.github.com/4ever2/7fbfb3bf843c4773c933c2fdf6315b5c)
