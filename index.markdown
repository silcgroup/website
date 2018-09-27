---
title: "SILC: Secure Interoperability, Languages, and Compilers"
is-index: true
---

Despite considerable progress in compiler verification in recent years, 
verified compilers are still proved correct under unreasonable assumptions about
what the compiled code will be linked with.  These assumptions range from no
linking at all---i.e., that compilation units are whole programs---to linking 
only with code compiled by the same compiler or from the same source 
language. Such assumptions contradict the reality of how we use compilers in
today's world of multi-language software where most software systems are
comprised of _components_ written in _different languages_ compiled by
_different compilers_ to a common target, as well as runtime-library
routines handwritten in the target language.

The SILC group is investigating techniques for principled compilation and secure
interoperability in a world of multi-language software.  Our recent
research on compositional compiler correctness (or correct compilation of
_components_) has led us to the conclusion that compositional compiler
correctness is fundamentally a multi-language problem: solutions to this 
problem demand both novel language design, in the form of principled FFIs for
languages, as well as compilers that preserve type specifications and
correctness of refactoring to ensure safe linking with code compiled from other
languages. 

Our long-term vision is to have verified compilers from languages as different
as C, ML, Rust, and Gallina---the specification language of the Coq proof
assistant---to a common low-level, richly (and gradually) typed WebAssembly-like
target language that enforces safe interoperability between components compiled
from more precisely typed, less precisely typed, and untyped source languages.  
In pursuit of this vision, we have made progress on challenging problems
in a number of areas relevant to correct and secure compilation in
the setting of multi-language software.

- Language interoperability, compiler correctness, and gradual typing:
We take the point of view that 
_compositional compiler correctness is a language
interoperability problem_ ([Ahmed 2015][#ahmed15:snapl]): embedding a
single-language fragment in a multi-language system affects the notion
of program equivalence that the compiler must use when reasoning about
the correctness of optimizations. But the precise notion of
equivalence can be adjusted by modifying the rules governing
interoperability (linking) in the multi-language system.  We have developed
a novel specification of   
compositional compiler correctness that requires that if a source 
component `s` compiles to a target component `t`, then `t` linked with
some arbitrary target code `t'` should behave the same as `s`
interoperating with `t'`.  To express the latter, we give a formal
semantics of interoperability between (high-level) source components
and (low-level) target code.  Our group has used _source-target
multi-language semantics_ for the verification of several compiler
transformations ([Perconti and Ahmed 2014][#perconti14], [Ahmed and Blume 2011][#ahmed11], [New et al. 2016][#new16], [Ahmed et al. 2016][#ahmed16]).

- We have also done work on _gradual typing_---which 
ensures safe interoperability between statically and dynamically typed
code---in the presence of 
polymorphism ([Matthews and Ahmed 2008][#matthews08], [Ahmed et al 2009][#ahmed09], [Ahmed et al 2011][#ahmed11], [New and Licata 2018][#newfscd18], [New and Ahmed 2018][#new18]).
Most recently, we have advocated that language designers equip their core
languages with _linking types_, a form of safe FFIs, to give programmers
the ability to specify how their components should interoperate with features
that cannot be expressed in their language.

- Type-preserving and refactoring-preserving compilation: 
We are investigating type-preserving compilers for a variety of languages to
prevent compiled components from being linked with ill-behaved target clients.
A type-preserving compiler preserves invariants and specifications from the
source program into the target program and can use these types to prevent 
linking with target code that might violate source-language type safety or
invalidate correctness of source-level refactoring.  We have done work on
type-preserving compilers for ML-like languages, as well as dependently typed
languages ([Bowman et al 2018][#bowmanpopl18], [Bowman and Ahmed 2018][#bowmanpldi18]).

- Proof methods for program equivalence:
Verifying correct compilation of _components_ requires proof methods for
establishing that the behavior of a source component is _equivalent_ to the
behavior of the compiled component. _Logical relations_ are a well-known
method for proving equivalence of program components, and our work on 
_step-indexed logical relations_ has shown how to scale the
method to realistic (typed and untyped) languages with features such
as ML- and Java-style mutable references, recursive types,
polymorphism, and
concurrency ([Ahmed 2004][#ahmedthesis], [Ahmed et al 2010][#ahmed10], [Ahmed 2006][#ahmed06], [Acar et al 2008][#acar08], [Ahmed et al 2009][#adr], [Dreyer et al 2009][#dreyer09], [Turon et al. 2012][#turon12]).
We have also shown how to extend 
logical relations to support reasoning about equivalence in several
mixed-language settings ([Matthews and Ahmed 2008][#matthews08], [Ahmed et al 2011][#ahmed11], [Perconti et al. 2014][#perconti14], [New et al. 2016][#new16], [Ahmed et al. 2016][#ahmed16], [Patterson et al. 2017][#patterson17]).

[#ahmed15:snapl]: http://www.ccs.neu.edu/home/amal/papers/verifcomp.pdf
[#perconti14]: http://www.ccs.neu.edu/home/amal/papers/voc.pdf
[#ahmed11]: http://www.ccs.neu.edu/home/amal/papers/epc.pdf
[#new16]: http://www.ccs.neu.edu/home/amal/papers/fabcc.pdf
[#ahmed16]: http://www.ccs.neu.edu/home/amal/papers/refcc.pdf
[#matthews08]: http://www.ccs.neu.edu/home/amal/papers/parpolyseal.pdf
[#ahmed09]: http://www.ccs.neu.edu/home/amal/papers/blame-all.pdf
[#ahmed11]: http://plt.eecs.northwestern.edu/blame-for-all/
[#newfscd18]: http://drops.dagstuhl.de/opus/volltexte/2018/9194/
[#new18]: http://www.ccs.neu.edu/home/amal/papers/graduality.pdf
[#bowmanpldi18]: http://www.ccs.neu.edu/home/amal/papers/closconvcc.pdf
[#bowmanpopl18]: http://www.ccs.neu.edu/home/amal/papers/cpscc.pdf
[#ahmedthesis]: http://www.cs.princeton.edu/research/techreps/TR-713-04
[#ahmed10]: http://portal.acm.org/citation.cfm?doid=1709093.1709094
[#ahmed06]: http://www.ccs.neu.edu/home/amal/papers/lr-recquant.pdf
[#acar08]: http://www.ccs.neu.edu/home/amal/papers/impselfadj.pdf
[#adr]: http://www.ccs.neu.edu/home/amal/papers/sdri.pdf
[#dreyer09]: http://www.ccs.neu.edu/home/amal/papers/lslr.pdf
[#dreyer11]: http://www.ccs.neu.edu/home/amal/papers/lslr-lmcs.pdf
[#turon12]: http://www.ccs.neu.edu/home/amal/papers/relcon.pdf
[#patterson17]: http://www.ccs.neu.edu/home/amal/papers/funtal.pdf

<div class="small">
## Research funding

- NSF: Principled Compiling and Linking for Multi-Language Software ([CCF-1816837](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1816837&HistoricalAwards=false), 10/2018-9/2021)
- NSF Graduate Research Fellowship for Aaron Weiss ([2017-2022](https://nsfgrfp.org))
- NSF: Foundations of Just-in-Time Compilation ([CCF-1618732](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1618732&HistoricalAwards=false), 9/2016-8/2018)
- NSF CAREER: Verified Compilers for a Multi-Language World ([CCF-1453796](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1453796&HistoricalAwards=false), 5/2015-4/2020)
- NSF: Secure Compilation of Advanced Languages ([CCF-1422133](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1422133&HistoricalAwards=false), 8/2014-7/2017)
- Google Faculty Research Award: Verified Compilers for a Multi-Language World, ([2014](https://ai.googleblog.com/2014/02/google-research-awards-winter-2014.html))
- NSF: Effectful Software Contracts ([CCF-1203008](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1117635&HistoricalAwards=false), 8/2011-7/2014)
</div>
