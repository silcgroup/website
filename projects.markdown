---
title: "SILC: Projects"
is-projects: true
---

<!-- ## Secure Compilation -->

<!-- dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut -->
<!-- labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud -->
<!-- exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute -->
<!-- irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat -->
<!-- nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa -->
<!-- qui officia deserunt mollit anim id est laborum.  -->

## Typed Compilation of Dependent Types
Dependently typed languages such as Coq have been used to specify and prove the full
functional correctness of software such as the CompCert optimizing C compiler ([Leroy 2009][#leroy2009]), the
CertiKOS operating system kernel ([Gu et al. 2015][#gu2015], [Gu et al. 2016][#gu2016]), and the
implementation of cryptographic primitives and protocols ([Barthe et al. 2009][#barthe2009], [Appel et
al. 2015][#appel2015]).

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
Dependent type theory has been used as a foundation for mathematics analogous to set theory.
Every standard compiler design decision must also avoid running afoul of fundamental problems such as
Girard's Paradox (the type-theoretic version of Russel's Paradox) and Gödel's incompleteness
theorem.
An impossibility result in 2002 showed that a known verified type-preserving compiler
pass---the standard typed CPS translation---leads to Girard's paradox when used on dependently typed
languages ([Barthe and Uustalu 2002][#barthe2002]).

Our group has recently developed an alternative CPS translation that avoids
 paradoxes ([Bowman et al. 2018][#bowman2018]), and has developed a scalable
 closure conversion pass.

[#appel2015]: http://doi.org/10.1145/2701415 "Verification of a Cryptographic Primitive: SHA-256"
[#barthe2002]: http://doi.org/10.1145/509799.503043 "CPS Translating Inductive and Coinductive Types"
[#barthe2009]: http://doi.org/10.1145/1480881.1480894 "Formal Certification of Code-based Cryptographic Proofs"
[#gu2015]: http://doi.org/10.1145/2775051.2676975  "Deep Specifications and Certified Abstraction Layers"
[#gu2016]: https://www.usenix.org/conference/osdi16/technical-sessions/presentation/gu  "CertiKOS: An Extensible Architecture for Building Certified Concurrent OS Kernels"
[#leroy2009]: http://doi.org/10.1007/s10817-009-9155-4 "A Formally Verified Compiler Back-end"
[#bowman2018]: https://www.williamjbowman.com/papers/#cps-sigma "Type-Preserving CPS for Σ and Π Types is Not Not Possible"

## Linking Types 

Large software systems are written using combinations of many languages. But
while some languages provide powerful tools for reasoning in the language, none
support reasoning across multiple languages. Indeed, the abstractions that
languages purport to present do not actually cohere because they do not allow
the programmer to reason solely about the code she writes. Instead, the
programmer is forced to think about the details of particular compilers and
low-level implementations, and to reason about the target code that her compiler
generates.

With Linking Types ([Patterson and Ahmed, 2017][lt-snapl]), we propose that language
designers incorporate linking into their language designs and provide
programmers a means to specify linking with behavior and types inexpressible in
their language. There are many challenges in how to design linking types,
depending on what features exist in the languages, but only through accepting
this challenge can we reach what has long been promised—an ecosystem of
languages, each suited to a particular task yet stitched together seamlessly
into a single large software project.
 
[lt-snapl]: https://dbp.io/pubs/2017/linking-types.pdf

 
## Semantics of Sound Gradual Typing

Gradually Typed languages enable programmers to smoothly migrate code
from dynamically typed to statically typed styles.
*Sound* gradually typed languages ensure that programmers have many of
the same strong reasoning principles that they have in a statically
typed language.
Our group studies how to develop dynamic enforcement schemes to
preserve the strong guarantees in the gradually typed setting that
advanced statically typed languages provide.

Our group has developed the first gradually typed language ([Ahmed,
Jamner, Siek, Wadler, 2017][grad-poly]) with parametric polymorphism
that is proven to satisfy the crucial parametricity property. This
proves that gradually typed languages can provide the strong reasoning
principles of encapsulation and information hiding that typed
functional programmers rely on.

More recently, we have developed a new type-theoretic and
category-theoretic formulation of gradual typing that helps to design
new gradually typed languages that automatically validate principles
such as extensionality or parametricity.  Furthermore, our general
theory allows us to prove strong meta-theorems for gradually type
languages: we can prove that classic definitions of contracts are the
*unique* definitions that satisfy extensionality and the *Gradual
Guarantee* (introduced in  [Siek, Vitousek, Cimini, Boyland, 2015][refined])

[grad-poly]: https://dl.acm.org/citation.cfm?doid=3136534.3110283
[refined]: http://snapl.org/2015/abstracts/full/Siek.html

## The Essence of Rust

Rust represents a major advancement in production programming languages because
of its success in bridging the gap between *high-level* application programming
and *low-level* systems programming. At the heart of its design lies a novel
approach to *ownership* that remains highly programmable.

In this ongoing work ([Weiss, Patterson, Ahmed, 2018][oxide-ml]), we are
designing a formal semantics for Rust that captures ownership and borrowing
without a separate lifetime analysis. This semantics models a high-level
understanding of ownership and as a result is close to source-level Rust (but
with some elaboration) which differs from the recent RustBelt effort that
essentially models MIR, a CPS-style IR used in the Rust compiler. Further,
while RustBelt aims to verify the safety of unsafe code in Rust’s standard
library, we model standard library APIs as primitives, which is sufficient to
reason about their behavior. This yields a simpler model of Rust and its type
system that we think researchers will find easier to use as a starting point
for investigating Rust extensions. Unlike RustBelt, we aim to prove type
soundness using *progress and preservation* instead of a Kripke logical
relation. Finally, our semantics is a family of languages of increasing
*expressive power*, where subsequent levels have features that are impossible
to define in previous levels. Following Felleisen, expressive power is defined
in terms of *observational equivalence*. Separating the language into different
levels of expressive power should provide a framework for future work on Rust
verification and compiler optimization.

[oxide-ml]: https://aaronweiss.us/pubs/ml18.pdf
