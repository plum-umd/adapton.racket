#lang racket
 
;; This file simply brings together everything implemented elsewhere
;; in the repo to one top-level file. All functions / structures / etc
;; defined in the repo are reachable from this file, other than anything
;; defined in "tests.rkt"

(require "graphing.rkt"
         "merge-sort.rkt"
         "adapton.rkt"
         "tools-for-testing.rkt"
         "data-structures.rkt"
         "memo-table-modification-tools.rkt")

;; -- tests
;; Defines a number of tests that ensure correctness and benchmark
;; this adapton implementation with merge-sort as the test algorithm

;; -- tools-for-testing
;; Defines a number of useful functions that automate things like printing
;; a readable list from a formatted lazy list, building lazy lists of 
;; different types, and mutating existing data.

;; -- memo-table-modification-tools
;; The bulk of the code that runs adapton. This file handles things like
;; maintaining successor / predecessor / creator relationships between 
;; articulation points, dirtying and cleaning of articulation points, 
;; and forcing articulation points. It's worth mentioning that this file
;; overrides racket's built-in definition of force. 

;; -- data-structures
;; This file defines the structures for nodes and cells, and also defines
;; several globals (hash-tables, a box for the stack, etc).

;; -- adapton
;; This file contains the functions that handle memoization of functions
;; and creation of thunks (which are then wrapped in nodes)

;; -- merge-sort
;; contains both an incremental implementation for merge-sort and a normal
;; implementation for merge-sort. 

;; -- graphing
;; contains a number of utilities for visualizing adapton.