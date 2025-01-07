# Minimal skeleton for developing a new Agda backend 

- The backend is defined in `src/Main.hs`.
- The `test/` directory contains an example compilation of `Test.agda` to `Test.txt`.


## Setup

Works with Coq 8.20.0 and MetaCoq 1.3.2.

## TODO

- [ ] Fix generation of Coq code
- [ ] Setup compilation to WASM/RUST
- [ ] Setup proper testing infrastructure
- [ ] Proper management of inductive declarations
- [ ] Support mutual definitions
- [ ] Support literals (ints and floats)

## References

- [Pierre Letouzey's thesis introducing LamndaBox](https://www.irif.fr/~letouzey/download/these_letouzey.pdf) (in French)
- [Verified Extraction from Coq to OCaml](https://github.com/yforster/coq-verified-extraction/)
  and its [accompanying paper](https://dl.acm.org/doi/10.1145/3656379)
- [Certified Erasure for Coq, in Coq](https://inria.hal.science/hal-04077552)
- [Syntax of LambdaBox in MetaCoq](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/ExAst.v) 
- [Erasure of types in MetaCoq](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/Erasure.v#L765)
- [Coq Extraction Pipeline](https://gist.github.com/4ever2/991007b4418b0ba44f2ee7ed51147e19)
- [MetaCoq Extracted Terms](https://metacoq.github.io/metacoq/html/MetaCoq.Erasure.EAst.html)
