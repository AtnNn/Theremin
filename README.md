# Theremin

Theremin is a research project for experimenting with:

* Generalised types
* Code search and versioning
* Polyglot programming
* Implicits in code
* Incremental programming
* 80x25 programming

Theremin is in a very early stage of development. The code in this
repository currently only contains an interpreter for a prolog-like
language.

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
generalised types, such as assisted theorem provers and novel
programming languages. These could be made easier to use with
mainstream programming languages.

## Code search and versioning

If the type of a function was a predicate as detailed as described
above, such a predicate could be used as query. The name of the
function would not matter, neither would the module it is imported
from or the version of the module or the quality of the code.

An import statement might look like this:

`import sort : sorted a (sort a) && time_complexity (sort a) < O(n * log(n))`

This raises concerns that could be addressed in different ways. Too
much abstraction makes debugging and reasoning about code very
difficult. The time complexity of a function is a very limited
restriction, the function may not be performant, it may have unwanted
side effects or it may use a lot of memory. A compiler would need an
indexed knowledge base to efficiently find the requested function. If
the knowledge base spanned multiple code bases there would be issues
with trust and transitive imports.

## Polyglot Programming

TODO

* Interoperability between languages
* Support for mainstream languages
* Storing program annotations and proofs separately
* Using static checking tools and anotations while staying compatible with other tools and languages
* Sharing knowledge between various tools
* Antiquotes in DSL

## Implicits in code

TODO

* Controlling abstraction level and implicit operations, such as overload resolution, memory allocation and implicit conversion.

## Incremental programming

TODO

* Reasoning about code at different abstraction levels
* Knowledge base for guided programming and managing TODOs
* Proving that the documentation is correct
* Or that the code correctly implements the documentation
* Self-documenting code
* Keeping clear and concise implementations next to more performant versions that may have more coupling and use unsafe and lower-level constructs

## 80x25 programming

TODO

* reasoning about software in short, independant parts
* 80x25 is just an arbitrary guideline and makes for a catchy name

## Authors

My name is Etienne Laurin. I write code for a living. I have
contributed to large projects in over a dozen languages from C++ to
Haskell, Prolog and JavaScript. I have used many dozens of other
different languages. I have tried improving some of these languages: I
built a tool to identify and remove duplicate code from large Scheme
codebases and I wrote the first implementation of deferred type errors
for GHC. I have ported complicated software between various platforms
and wrote and maintained large build systems. I believe programming
can be improved and that we already have most of the tools available
to do so.

## Inspiration

Some of the ideas that make up Theremin are inspired by these
languages and projects:

* Ciao (http://ciao-lang.org/)
* Idris (http://www.idris-lang.org/)
* Erlang (http://www.erlang.org/)
* GCLA (https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_36.html)
* Dafny (http://research.microsoft.com/en-us/projects/dafny/)
* SAW (http://saw.galois.com/)
* Infer (http://fbinfer.com/)
* Seahorn (https://github.com/seahorn/seahorn)

