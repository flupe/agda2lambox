# agda2lambox

An [Agda] backend to generate [MetaCoq] λ□ (LambdaBox) programs
for further (verified) extraction to WASM or Rust.
The backend builds off Agda 2.7.0.1.
Compatible with Coq 8.19.0, MetaCoq 1.3.1 and CertiCoq 0.9.

[Agda]: https://github.com/agda/agda
[MetaCoq]: https://metacoq.github.io/

To install the backend, setup GHC (tested with `9.10.1`) and cabal.

```
git clone git@github.com:omelkonian/agda2lambox.git
cd agda2lambox
cabal install
```

This will take a while, as it has to (recursively) clone the Agda repo 
and compile from source.

Then you're good to go.

```
agda2lambox [AGDAFLAGS] [--out-dir DIR] [--typed] FILE
```

## Setup

The backend generates `.v` and `.txt` files that contain the extracted λ□ environment.
To check what's generated, setup CertiCoq and compile the minimal Coq prelude.

```
opam pin add certicoq 0.9+8.19
coq_makefile -f _CoqProject -o CoqMakefile
make -f CoqMakefile
```

## Status

- Most standard Agda features are supported:
  - Mutually-defined datatypes and record types.
  - Mutually-defined functions.
  - Importing modules.
    The backend transitively compiles all required definitions, and only those.
  - Nat literals. Note: currently literal nat patterns are restricted to `0`.
    You cannot match on `5`, for now.

- Typed λ□ environments can be generated with the `--typed` flag.
  - Inductive and record types get as many type variables as they have parameters.
  - constants also get some of their type arguments lifted to type variables,
    when possible.

## TODO

- [ ] Type aliases.
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
