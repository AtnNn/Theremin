# Theremin

Theremin is programming language and a static analysis tool built to
experiment with:

* Predicates as types
* Code search, versioning
* Polyglot programming

Theremin is in a very early stage of development. The code in this
repository currently only contains an evaluator for a prolog-like
language.

Join the discussion on irc.freenode.net #theremin

Find the code at http://github.com/atnnn/theremin

## Predicates as types

In Haskell, the type of a `sort` function might be `Ord a => [a] ->
[a]`. Ideally, a programmer should be able to tell the compiler that
the type of `sort` is `sorted a (sort a)`, where `sorted` could
be defined as `sorted a b = bag b == a && ordered b`. The compiler
could even be told that `time_complexity (sort n) = O(n * log n)`.

This has already been done in languages such as Agda [1]. Proving
things in Agda has disadvantages: It requires modifying the program
things are being proven about by using types as proofs. Proofs easily
break if they do not follow exactly the structure of the program. With
Theremin, different types of notations for expressing proofs will be
explored to make proofs shorter and more natural to write.

[1] https://gist.github.com/twanvl/5635740

Theremin aims at solving some of these problems by removing types
entirely and using more general predicates to reason about
programs. Theremin will use static analysis to build a knowledge base.
The knowledge base will also contain code annotations and static
asserts. Different third-party solvers will be used to check the
consistency of the knowledge base.

## Code search and versioning

A detailed predicate about a function could describe the arguments,
the return value, the time complexity and the side effects. Such a
predicate could be used as query. The name of the function would not
matter, neither would the module it is imported from or the version of
the module or the quality of the code.

An import statement might look like this:

`import sort : sorted a (sort a) && time_complexity (sort a) < O(n * log(n))`

This raises many concerns that Theremin will attempt to resolve. Too
much abstraction makes debugging and reasoning about code very
difficult. The time complexity of a function is a very limited
restriction. The function may not be performant, it may have unwanted
side effects or it may use a lot of memory. A compiler would need an
indexed knowledge base to efficiently find the requested function. If
the knowledge base spanned multiple code bases there would be issues
with trust and transitive imports.

## Polyglot Programming

Interoperability with other languages is one of the main concerns of
Theremin. Although it is currently only a playground for ideas, the
goal of Theremin is to be useful for everyday programmers. It must
therefore work with mainstream languages.

## Authors

I am currently the sole author of Theremin. I use to experiment the ideas
expressed above. Please help by contributing code, ideas or criticism.

My name is Etienne Laurin. I write code for a living. I have
contributed to large projects in over a dozen languages from C++ to
Haskell, Prolog and JavaScript. I have used many dozens of other
different languages. I have tried improving some of these languages: I
built a tool to identify and remove duplicate code from large Scheme
codebases and I wrote the first implementation of deferred type errors
for GHC.

## Name

The name Theremin was suggested by my co-workers Michael during a
brainstorming session. It was chosen because it is a well known word
that is easy to pronounce in English and because it lets me use the
extension `the` for Theremin source files.

## Inspiration

Some of the ideas that make up Theremin are inspired by these
languages and projects:

* Ciao (http://ciao-lang.org/)
* Idris (http://www.idris-lang.org/)
* Erlang (http://www.erlang.org/)