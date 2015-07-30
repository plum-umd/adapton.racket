#lang racket

;; This file contains the data structures that are used in adapton.rkt,
;; and is also needed in most of the tools/etc files.

(provide (all-defined-out))

;; =========== GLOBAL VARIABLES ===========

;;create a top level table for nodes
(define *memo-table* 
  (make-hasheq))

;; create a top level table for cells
(define *cells*
  (make-hasheq))

;; stack 
(define stack (box '()))

;; stack for create edges
(define create-stack (box '(00000)))

;; cell counter
(define cell-counter (box 0))

;; ======================== DATA STRUCTURES =====================
;; node structure
(struct node (id dirty successors predecessors creator thunk result))

;; USER NODE
;; serves as root node for all user forced nodes
#;(hash-set! *memo-table* 00000 (node 00000 
                                    #f 
                                    '() 
                                    '() 
                                    0 
                                    (λ () 
                                      (get-unforced-list-from-mergesort
                                       (force (merge-sort (build-input 8))))) '()))
;(write-to-graph (format "[change]add node~n[node ~a red]~n" 00000))

;; edge structure
;; type is one of 'n (node) or 'c (cell),
;; id is the target of the edge
;; result is the this edge's knowledge of the old result
;; of its target.
(struct edge (type id result))

;; cell structure
(struct cell (id box predecessors dirty))