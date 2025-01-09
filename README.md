# agda2lambox

An [Agda] backend to generate [MetaCoq] λ□ (LambdaBox) programs
for further (verified) extraction to WASM or Rust.
The backend builds off Agda 2.7.0.1.

[Agda]: https://github.com/agda/agda
[MetaCoq]: https://metacoq.github.io/

## Setup

Compatible with Coq 8.20.0 and MetaCoq 1.3.2.

## TODO

- [x] Fix generation of Coq code
- [x] Add Coq pretty-printing
- [x] Support mutual definitions
- [x] Support one-inductive
- [ ] Make pretty-printer prettier
- [ ] Ensure well-formedness of generated programs inside Coq
- [ ] Add pragma for program selection
- [ ] Evaluate λ□ programs from inside Coq to start testing
  - [ ] Possibly salvage an existing interpreter
  - [ ] Or implement our own...
- [ ] Support mutual inductives
- [ ] Support (one-inductive) records
- [ ] Support mutual (possibly inductive) records
- [ ] Better error-reporting
- [ ] "Support" modules
- [ ] Check support for Agda-specific edge cases
  - [ ] Pattern-matching lambdas
  - [ ] With-generated lambdas
  - [ ] Module applications
  - [ ] Projection-like
- [ ] Support literals (ints and floats)
- [ ] Setup compilation to Wasm/Rust using Certicoq
  - [ ] May require generating λ□ terms for an older MetaCoq
- [ ] Setup proper testing infrastructure

## References

- [The MetaCoq Project](https://github.com/MetaCoq/metacoq)
- [The CertiCoq Compilation pipeline](https://github.com/CertiCoq/certicoq/wiki/The-CertiCoq-pipeline)
- [CertiCoqWASM](https://github.com/womeier/certicoqwasm)
- [Pierre Letouzey's thesis introducing λ□](https://www.irif.fr/~letouzey/download/these_letouzey.pdf) (in French)
- [Verified Extraction from Coq to OCaml](https://github.com/yforster/coq-verified-extraction/)
  and its [accompanying paper](https://dl.acm.org/doi/10.1145/3656379)
- [Certified Erasure for Coq, in Coq](https://inria.hal.science/hal-04077552)
- [Syntax of LambdaBox in MetaCoq](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/ExAst.v) 
- [Erasure of types in MetaCoq](https://github.com/MetaCoq/metacoq/blob/coq-8.20/erasure/theories/Typed/Erasure.v#L765)
- [Coq Extraction Pipeline](https://gist.github.com/4ever2/991007b4418b0ba44f2ee7ed51147e19)
- [MetaCoq Extracted Terms](https://metacoq.github.io/metacoq/html/MetaCoq.Erasure.EAst.html)
- [Extraction Example](https://gist.github.com/4ever2/7fbfb3bf843c4773c933c2fdf6315b5c)
