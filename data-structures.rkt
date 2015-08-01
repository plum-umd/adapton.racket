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

;; the stack holds a list of articulation points (nodes or cells).
;; the top element of the stack is the "currently active" articulation
;; point. the stack is used to keep track of successor/predecessor 
;; relationships
(define stack (box '()))

;; create-stack mirrors the stack but has the element 00000 on the bottom.
;; when a node is created an edge is created between the new node and its
;; creator. having a seperate stack for create edges allows us to have 
;; creation edges between a "user node (00000)" and nodes with no predecessor.

;; create-edges are necessary for purge-unreachable to work properly, and require
;; the user to run the initial input by creating a thunk and storing it in 
;; the node 00000. The user then forces the user node to run his or her code.

;; For most of the functionality provided by adapton, this is not necessary.
;; You should attempt to familiarize yourself with most of the functionality
;; provided by adapton before worrying about create-edges or user nodes. 
(define create-stack (box '(00000)))

;; cell counter keeps track of the id of the last created cell, and is 
;; incremented each time a cell is created.
(define cell-counter (box 0))

;; ======================== DATA STRUCTURES =====================
;; node structure
(struct node (id dirty successors predecessors creator thunk result))

;; USER NODE
;; serves as root node for all user forced nodes, see above
#;(hash-set! *memo-table* 00000 
             (node 00000 
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
;; result is the this edge's knowledge of the old result of its target.
(struct edge (type id result) #:transparent)

;; cell structure
(struct cell (id box predecessors dirty) #:transparent)