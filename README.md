# Theremin

Theremin is a programming language and a code analysis tool built to
experiment with:

* Predicates as types
* Code search and versioning
* Polyglot programming

Theremin is in a very early stage of development. The code in this
repository currently only contains an interpreter for a prolog-like
language.

Join the discussion on irc.freenode.net #theremin

Find the code at http://github.com/atnnn/theremin

## Predicates as types

Types are predicates over one or more variables, values or functions.

A variable's type helps to determine its structure, representation and
constraints. A natural number could be structured as `s(s(s(z)))`,
represented as machine word, and have the constraint that it not be 0.

The type of a function can include constraints over one or multiple of
its arguments, but can also depend on the function's AST, call graph,
or the result of abstract interpretation or other analyses.

Types can be expressed as run-time assertions that are removed from
the code if they can be proved to always be true.

## Code search and versioning

If the type of a function was a predicate as detailed as described
above, such a predicate could be used as query. The name of the
function would not matter, neither would the module it is imported
from or the version of the module or the quality of the code.

An import statement might look like this:

`import sort : sorted a == sort a && time_complexity (sort a) < O(length a * log(length a))`

## Polyglot Programming

Although this project is currently only a playground for ideas, the
goal of Theremin is to be useful for everyday programmers. It will
therefor need a good foreign language interface and be able to parse
and analyse code written in at least one mainstream language.

## Other Ideas

* Controlling abstraction level and allowed implicit operations, such as overload resolution, memory allocation, memory layout and implicit conversion.
* Storing program annotations and proofs separately
* Antiquotes in DSL

## Authors

I am currently the sole author of Theremin. I use to experiment with
ideas. Please help by contributing code, ideas or criticism.

My name is Etienne Laurin. I have contributed to large projects in
over a dozen languages from C++ to Haskell, Prolog and JavaScript and
I have tried many dozens of other languages. But I was never
satisfied. Some of the programming language projects I wrote are: a
tool to factor our duplicate code from large Scheme codebases, a
Pro*PL/1 to PL/SQL converter and an early prototype implementation of
deferred type errors for GHC.

## Name

The name Theremin was suggested by my co-workers Michael, Michael and
Daniel during a brainstorming session. It was chosen because it is a
well known word that is easy to pronounce in English and because it
lets me use the extension `the` for Theremin source files.

## Inspiration

Some of the ideas that make up Theremin are inspired by these
languages and projects:

* Ciao (http://ciao-lang.org/)
* Idris (http://www.idris-lang.org/)
* GCLA (https://sicstus.sics.se/sicstus/docs/3.7.1/html/sicstus_36.html)
* Dafny (http://research.microsoft.com/en-us/projects/dafny/)
* SAW (http://saw.galois.com/)
* Infer (http://fbinfer.com/)
