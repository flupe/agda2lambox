Notes and questions on the ongoing implementation of `agda2lambox`.

- MetaCoq's λ□ programs can be well-formed in different ways 
  --- [parametrized by `EEnvFlags`][envflags].

  However, CertiCoq seems to expect a specific kind of λ□ programs, in
  particular as input to the translation to λ□-Mut. I've inferred that:

  - Constructors should be fully-applied (and thus eta-expanded).
  - There should be *no projections* remaining.
  - Datatypes and record types should not be parametrized.

  Is this correct? Where exactly is this requirement stated?

- As `agda2lambox` is *not* going to be verified, it should strive to be
  as straightforward as possible.
  Currently I had to implement eta-expansion of constructors to comply to what
  *I think* CertiCoq expects (See `Agda.Utils (etaExpandCtor)`).
  The code isn't very complex and I'm almost convinced it's correct,
  but to me it falls in the category of transformations that should happen
  on the Coq side.

  Conveniently, MetaCoq already provides a verified *transformation* on λ□ 
  to perform this eta-expansion of constructors.
  See [`EConstructorsAsBlocks`][ctorblocks].

  *However*, this transformation is implemented on `eprogram`,
  which is a slightly more efficient way to encode programs (where the
  environment is not a flat association list anymore).
  *But* CertiCoq's pipeline ony wants regular `program`s.

  - Is there a way to convert a `program` to an `eprogram`?
  - Is there a way to get CertiCoq to work on `eprograms`s?

- How would we go about making the backend incremental?

- Is it reasonnable to pack all the Agda codebase into a single output `.v`
    file?

- Can CertiCoq generate multiple files of Rust? Mutiple blobs of WASM?

- How does CertiCoq handles primitives/axioms? How does it work for types?

- How would we make it easier to do the plumbing from inside Agda?

[ctorblocks]: https://github.com/MetaCoq/metacoq/blob/v1.3.1-8.19/erasure/theories/EConstructorsAsBlocks.v
[envflags]: https://github.com/MetaCoq/metacoq/blob/ea3ed3c4b0d05508ce744f17a56c880c5f47c816/erasure/theories/EWellformed.v#L55

