# Theremin

Theremin is a research project for experimenting with:

* Generalised types
* Code search and versioning
* Polyglot programming
* Implicits in code
* Incremental programming
* Free as in monad
* Programming environment

I am exploring these loosely related ideas in an attempt to make
software development faster, easier and more fun. Despite the fact
that this document reads like a manifesto or a list of eccentric
ideas, my goal is to end up with pragmatic tools that I can use every
day. This repository already contains some code that has been useful
to me.

If you are interested in these ideas, join me on irc.freenode.net #theremin

Find the code at http://github.com/atnnn/theremin

> There are only two hard things for the human mind: re-evaluating
  beliefs, putting thoughts into words and knowing when to stop.

## Generalised types

Most programming languages have a very restricted concept of types.
In general, I expect the type of a function or variable to fully
describe its public interface, while the body or value would be a
private implementation or instance of the type.

However, In most languages, the types correspond only to the
information needed by the compiler or interpreter to generate target
code or select the correct branch at runtime. In some languages types
can also help programmers avoid certain classes of bugs.

A variable or function, in the context or a larger body of code, will
have the following characteristics. All of these, at some level, could
make up its type:

* Representation or ABI
* Value or implementation
* Documentation
* Other constraints such as tests, assertions, pre-conditions,
  post-conditions and invariants

Most of these characteristics only serve as hints to developers or as
separate or run-time checks. These hints could be checked by machine
to reduce cognitive load. These checks could be performed earlier to
improve correctness and performance.

The syntactic difference between terms and types in most languages
increases the dichtomy between constraints that are checked and those
that aren't. This situation could be improved by using the same
language for terms and types.

There are already a wealth of tools that can check and work with these
generalised types, such as assisted theorem provers, constraint
solvers and novel programming languages. These could be made easier to
use with mainstream programming languages.

## Code search and versioning

If the type of a function was as detailed as described above, such a
type could be used as query. The name of the function would not
matter, neither would the module it is imported from or the version of
the module or the quality of the code.

An import statement might look like this:

`import sort : sorted (sort a) && bag(a) = bag(sorted(a)) && time_complexity (sort a) < O(len(a) * log(len(a)))`

This raises concerns that could be addressed in different ways. Too
much abstraction makes debugging and reasoning about code very
difficult. The time complexity of a function is a very limited
restriction, the function may not be performant, it may have unwanted
side effects or it may use a lot of memory. A compiler would need an
indexed knowledge base to efficiently find the requested function. If
the knowledge base spanned multiple code bases there would be issues
with trust and transitive imports.

## Polyglot programming

Almost all programming languages have a vast amount of concepts and
properties in common, but the little details in semantics and
implementation make them very incompatible. This incompatibility can
be bridged, see for example the vast amount of compilers targetting
JavaScript. This could be done in a more general way.

Porting code to a different language could be a lot easier. Using
libraries written in a different language could be a lot easier. It
could be possible to mix different languages in the same source file,
using quotes and antiquotes and/or a custom pre-processor.

Many beneficial tools and features developped for academic languages
could be ported to work in mainstream languages. There needs to be
better frameworks and better tools available to encourage the
unobstrusive and incremental adoption of better tools and features in
mainstream languages, as well as a common ground for these tools and
features to interoperate harmoniously and share analyses.

## Implicits in code

The following are examples of operations that can, to various degrees,
be surrendered to the programmer or claimed by a system, language or
library:

* Memory allocation
* Scheduling
* Concurrency
* Overload resolution
* Type inference
* Type conversion
* Type representation
* Optimisations
* Default initialisation
* Import resolution

Programmers could be allowed more control over these operations, to
make them entirely explicit or to allow better fine tuning and
customisation or to hide them entirely. Either way, compilers,
interpreters and debuggers could be fitted with much better better
flags, pragmas and API for exposing and examining implicit operations.

The choices that are made for these implicit operations are often
ambiguous and highly dependant on the environment. In addition to
being exposed, these choices could also be stored (inline or in a
separate file). In future runs, this data could be used as a guide for
new choices and as a source of warnings when the choices change (such
as a diferent version or type of an import, or a missing
optimisation).

Code that expresses things like instructions, operations and
representations would often be better if the compiler did not hide any
implicit choices about those operations and representations. But when
code expresses higher-level ideas or data, it is often better for the
compiler to hide low-level details and for the programmer not to hide
implicit constraints.

## Incremental programming

Source code is rarely ever complete. Most code bases come with a large
amount of TODOs, issues, missing documentation, missing code,
unoptimised code, experimental features, debug flags, misunderstood
hacks, version control and many other indications of a work in
progress.

Version control could allow tracking issues, features and fixes
directly in addition to plain branches. Diffs could be improved by
making them aware of syntax, refactoring patterns and of the
multi-file nature of changes. They could also be allowed to commute.

Code made ugly by optimisations could live next to a clean readable
implementation, and a proof of the equivalence could be available.

The same could be true for issues and documentation, they could be an
integral part of the code. Documentation for a feature could be linked
to the code that implements the feature. Issues and fixes could be
linked together through version control.

Language or preprocessors could provide better syntactical,
zero-overhead abstractions to encourage writing self-documenting code.

Tools and editors could better encourage reasoning about software in
short, independant parts. I call this 80x25 programming.

## Free as in monad

Better tools could be produced for free.

Free FFI bindings can be generated from a header file. Free
documentation can be generated from tests and well-documented code.

If I take this idea further, a free compiler could even be generated
from any interpreter, or a free interpreter from a compiler.

## Programming environment

The requirements and expectations for one-off scripts, debugging code,
exploratory programming and interactive programming are very different
from the requirements of code that has to be re-used, maintained or
shared. But in practice, all these types of code have to mix.

It should be easy to write and test code interactively, and then easy
to re-format, verify, document and save it for re-use. Conversly, it
should be easy to add temporary sections of code that can access
private fields, that don't trigger any warnings and that have access
to the current state of the program.

It should be possible to examine all static constructs
dynamically. Static and dynamic environments could be available in a
platform-independent way. Source code could be read as if in a
homoiconic language. Algorithms used by the compiler could all be
exposed as a library, such as template instantiation, type inference
and linking.

All this could be done with awareness of the whole codebase, such as
generated code, or cumulative flags taken from the default compiler
flags, the environment and custom makefiles.

## Author

My name is Etienne Laurin. I write code for a living. I have
contributed to large projects in over a dozen languages from C++ to
Haskell, Prolog and JavaScript. I have used many dozens of other
different languages. I have tried improving some of these languages:
on college I built a tool to identify and remove duplicate code from
large Scheme codebases and later I wrote the first implementation of
deferred type errors for GHC. I have ported complicated software
between various platforms and wrote and maintained large build
systems. I believe programming can be improved and that we already
have most of the tools available to do so.

## Inspiration

Some of these ideas are inspired by these languages and projects:

* Ciao (http://ciao-lang.org/)
* Idris (http://www.idris-lang.org/)
* Erlang (http://www.erlang.org/)
* GCLA (https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_36.html)
* Dafny (http://research.microsoft.com/en-us/projects/dafny/)
* SAW (http://saw.galois.com/)
* Infer (http://fbinfer.com/)
* Seahorn (https://github.com/seahorn/seahorn)
* Parrot Compiler Tools (http://trac.parrot.org/parrot/wiki/Languages)
* Kythe (https://www.kythe.io/)
* Leon (https://leon.epfl.ch/)
* Why3 (http://why3.lri.fr/)
* K (http://www.kframework.org/)
* Babelsburg (https://github.com/babelsberg/babelsberg-r)
