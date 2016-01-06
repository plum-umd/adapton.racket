#lang scribble/doc

@(require (for-label adapton))
@begin[(require scribble/manual)
       (require scribble/eval)
       (require scribble/basic)]

@(define evaluator
   (let ([evaluator (make-base-eval)])
     (evaluator '(require "main.rkt"))
     evaluator))

@title[#:tag "top"]{@bold{Adapton}: Composable, Demand-Driven Incremental Computation}

by Clayton Mentzer and Matt Hammer {cmentzer at ccs dot neu dot edu}

Adapton is a library for demand driven incremental computation and dynamic programming
The library provides drop-in replacement forms for defining Racket functions that memoize
their results and record their computation graphs. In addition, it provides tools
for leveraging articulation points to improve performance of algorithms on large inputs
and the tools to mutate those inputs. 

@table-of-contents[]

@defmodule[adapton]{}

@section[#:tag "intro"]{Example: Fibonacci}

A typical example of a dynamic programming problem is computing the Fibonacci numbers,
whose simplest implementation involves a heavy amount of duplicated computation. By
simply defining the function with define/memo, previously computed answers
are cached, avoiding the duplicated computation. 

calling a funciton defined with define/memo returns a node, which is a structure that 
contains a thunk. Nodes are one  of the two types of articulation points in Adapton, 
the other being cells. We'll get to Cells in a moment. Forcing a node will force the
thunk contained within that node, and also perform a number of steps to keep track of
relationships between articulation points.

each recursive call to fib will also return a node, which means we need to force that 
node before we can use it. Each node created is placed into a global level hash-table 
called "*memo-table*". Then, each time a node is created we can check if it exists in 
memo-table already, and use its cached result. 

@defexamples[#:eval evaluator
                    (define (fib n)
                      (if (<= n 1) 
                          1 
                          (+ (fib (- n 1)) (fib (- n 2)))))
                    (time (fib 35))
                    (define/memo (fib n)
                      (if (<= n 1) 
                          1 
                          (+ (force (fib (- n 1))) (force (fib (- n 2))))))]

The other kind of articulation point in Adapton is a cell. Cells are structures that 
contain a mutable box for atomic data. The difference between cells and nodes is that a
cell will never have any successors (that is, the data contained in a cell will never force
another articulation point). We use cells to contain our input data, so that when a node 
forces a cell to get the atomic data out of it, that cell is flagged as a successor to that
node, and we say that the node is dependent upon that cell. 

We use these dependencies to build a Directed Computation Graph (DCG), which is a model
of the control flow of the program. When the data contained within a cell is mutated, we 
can determine based on the DCG which nodes have been invalidated (dirtied), and recomputing
only those nodes will correct our now invalid result. 

This allows us to only re-compute a fraction of the computations that we originally 
performed to achieve a new correct result. The merge-sort example in the adapton library
uses cells liberally. 

@section[#:tag "forms"]{Forms}

Just like the function definition forms in PLT Scheme, the formals list of a memoized function
may be a single identifier, a proper list of identifiers, or an improper list of identifiers.

@schemegrammar[formals id () (id . formals)]

@subsection[#:tag "definitions"]{Definition Forms}

@defform[#:id define/memo (define/memo (name . formals) body ...)]{
                                                                   Defines a memoized function @scheme[name] with formal arguments @scheme[formals] and function body forms @scheme[body ...].
                                                                                               Inputs are cached in a hash table and looked up with @scheme[eq?].}

@defform[#:id force (force articulation-point)]{
                                                Performs some amount of work to extract the value from an articulation point. Also keeps track of dependencies between articulation points
                                                and handles memoization, and dirtying and cleaning of articulation points. NOTE: This function overrides Racket's built-in force function!}
