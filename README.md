Racket E-graph Library
======================

Equivalence graphs are a data structure for compactly storing many
equivalent expressions. The
[Denali](https://dl.acm.org/citation.cfm?id=512566) paper is a good
introduction to the general idea and one of its uses. Regraph is a
battle-tested, production-ready implementation of this data structure
in pure Racket, originally developed for the
[Herbie](https://herbie.uwplse.org/) project, which has since migrated
to the Rust-based [E-Graphs Good](https://github.com/mwillsey/egg) library.

Installing
----------

Regraph can be installed directly from package servers:

    raco pkg install regraph

You can then include Regraph with

    (require regraph)

Creating Equivalence Graphs
---------------------------

Regraph considers two types of expressions: atomic expressions, which
can be anything except a list, and function applications, which are a
list where the first element is the function and later elements are
arguments. It is convenient if `#f` is not a valid expression.

The Regraph API is fairly simple:

- `(make-regraph exprs #:limit N)` creates an equivalence graph,
  initially seeded with the expressions from `exprs`, and limited to
  `N` nodes, which in practice should be around 10 000.
- `(regraph-count rg)` and `(regraph-cost rg)` measure the physical
  and logical size of the equivalence graph.
- `(regraph-extract rg)` returns expressions equivalent to those in
  `exprs`, but optimized to have lower cost.

`rg` above represents the equivalence graph returned by
`make-regraph`. In Regraph, cost is just the total size of the
expression, counting atomic expressions as 1 and function applications
as 1 plus the size of the arguments.

Adding to Equivalence Graphs
----------------------------

However, if you just apply these methods, you won't get interesting
results. To do that, you'll need to add expressions and expression
equivalences to the equivalence graph using "phases". Regraph provides
four:

- `(rule-phase ipats opats)` applies rewrite rules, given by patterns,
  wherever they match in the equivalence graph. The rules are given by
  two parallel lists, which contain the input and output patterns of
  each rule.
  
  A pattern is like an expression, but symbols represent
  pattern variables. For example, the pattern `(+ a b)` matches all
  expressions with `+` applied to any two arguments, while `(* a (/ 1
  a))` matches all products of one thing and `/` applied to the atomic
  expression `1` and that thing.

- `(precompute-phase fn)` calls `(fn op args ...)` for each function
  application expression whose arguments are equivalent to atomic
  expressinos. `fn` should return either `#f` (meaning that no answer
  can be computed) or an expression, in which case that expression
  will me made equivalent to the original. This is particularly useful
  for implementing various forms of constant-folding.
  
- `prune-phase` deletes all expressions from the equivalence graph
  that are equivalent to an atomic expression, since atomic
  expressions are lower-cost than any non-atomic expressions. Calling
  this often may prevent Regraph from finding the lowest-cost
  equivalent to an expression, but will speed up rule application. It
  does not affect `regraph-count`.

- `extractor-phase` updates the lowest-cost versions of each
  expression. Run this before calling `(regraph-extract)` or
  `(regraph-cost)` to get accurate results.

Each phase can be applied to an equivalence graph with `(phase rg)`.
Phases will not do anything once the equivalence graph hits the node
limit it was defined with.

The best way to use Regraph is to create the equivalence graph with
`make-regraph`, then apply whatever phases you'd like in a loop until
`regraph-count` either ceases to change or hits the defined limit, and
then call `regraph-extract` to get the best versions of the original
expressions.

Example Uses
------------

For simplifying algebraic expressions, define a set of rewrite rules
from algebraic identities. Then create an equivalence graph, apply the
rules repeatedly, and extract the results.

For solving congruence closure problems, create an equivalence graph
with the two sides of the equality being tested, add each given
equality as a rewrite rule, and test the extracted results for exact
equality.
