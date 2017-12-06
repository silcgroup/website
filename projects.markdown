---
title: "SILC: Projects"
is-projects: true
---

## Secure Compilation

dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut
labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute
irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
qui officia deserunt mollit anim id est laborum. 

## Typed Compilation of Dependent Types
Dependently typed languages such as Coq have been used to specify and prove the full
functional correctness of software such as the CompCert optimizing C compiler ([Leroy 2009](#leroy2009)), the
CertiKOS operating system kernel ([Gu et al. 2015](#gu2015), [Gu et al. 2016](#gu2016)), and the
implementation of cryptographic primitives and protocols ([Barthe et al. 2009](#barthe2009), [Appel et
al. 2015](#appel2015)).

Unfortunately, these verification efforts are all easily undone when linking with external components.
Typically, Coq programs are translated to OCaml, a high-level functional language with a strong type
system (but without dependent types).
This translation discards the Coq specifications, so OCaml can no longer enforce the
invariants Coq relied on when verifying the source program.
It is simple to construct a small verified Coq function that, when compiled and linked with well-typed
code in OCaml, promptly *jumps to an arbitrary location in memory*.

The key to preventing programs from being linked with ill-behaved external components is
*type-preserving compilation*.
Types express interfaces, invariants, and specifications.
A type-preserving compiler preserves all typing information from the source program into the target
programs.
Safe linking is guaranteed through type checking in the target language.

Type preservation for dependent types is a fundamentally hard problem and has been stalled for fifteen
years.
Dependent type theory has been used a foundation for mathematics analogous to set theory.
Every standard compiler design decision must also avoid running afoul of fundamental problems such as
Girard's Paradox (the type-theoretic version of Russel's Paradox) and Gödel's incompleteness
theorem.
An impossibility result fifteen years ago showed that a known verified type-preserving compiler
pass---the standard typed CPS translation---leads to Girard's paradox when used on dependently typed
languages ([Barthe and Uustalu 2002](#barthe2002)).

Our group has recently developed alternative type-preserving versions of the two major front-end
compiler passes, including an alternative CPS translation that avoids Girard's paradox.

#### Bibliography
- <a name="appel2015">Appel, A. W.</a>
  Verification of a Cryptographic Primitive: SHA-256
  ACM Transactions on Programming Languages and Systems (TOPLAS), 2015, 37
  http://doi.org/10.1145/2701415
- <a name="barthe2002">Barthe, G. & Uustalu, T.</a>
  CPS Translating Inductive and Coinductive Types
  Workshop on Partial Evaluation and Semantics-based Program Manipulation (PEPM), 2002
  http://doi.org/10.1145/509799.503043
- <a name="barthe2009">Barthe, G.; Grégoire, B. & Zanella-Béguelin, S.</a>
  Formal Certification of Code-based Cryptographic Proofs
  Symposium on Principles of Programming Languages (POPL), 2009
  http://doi.org/10.1145/1480881.1480894
- <a name="gu2015">Gu, R.; Koenig, J.; Ramananandro, T.; Shao, Z.; Wu, X. (n.; Weng, S.-c.; Zhang, H. & Guo, Y.</a>
  Deep Specifications and Certified Abstraction Layers
  Symposium on Principles of Programming Languages (POPL), 2015
  http://doi.org/10.1145/2775051.2676975
- <a name="gu2016">Gu, R.; Shao, Z.; Chen, H.; Wu, X. (N.; Kim, J.; Sjöberg, V. & Costanzo, D.</a>
  CertiKOS: An Extensible Architecture for Building Certified Concurrent OS Kernels
  Symposium on Operating Systems Design and Implementation (OSDI), 2016
  https://www.usenix.org/conference/osdi16/technical-sessions/presentation/gu
- <a name="leroy2009">Leroy, X.</a>
  A Formally Verified Compiler Back-end
  Journal of Automated Reasoning, 2009, 43
  http://doi.org/10.1007/s10817-009-9155-4

## Linking Types 

Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur,
adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et
dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum
exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi
consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit
esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo
voluptas nulla pariatur?
 
 
## Semantic Foundations for Gradual Typing

dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut
labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute
irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat
nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa
qui officia deserunt mollit anim id est laborum.



