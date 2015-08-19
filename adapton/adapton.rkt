#lang racket

;; This file contains the core of adapton, consisting of:
;; - definition of functions memoized for adapton
;; - creation of nodes when those functions are called
;; - force for nodes and cells

(require (prefix-in r: (only-in racket delay force equal-hash-code))
         rackunit
         "memo-table-modification-tools.rkt"
         "graphing.rkt"
         "data-structures.rkt")
;; This file contains the core of adapton, consisting of:
;; - definition of functions memoized for adapton
;; - creation of nodes when those functions are called
;; - force for nodes and cells

(require (prefix-in r: (only-in racket delay force equal-hash-code))
         rackunit
         "memo-table-modification-tools.rkt"
         "graphing.rkt"
         "data-structures.rkt")

(provide (all-defined-out))

;; ========================================

;; define/memo creates a memoized version of a function
;; any function that you want to be tracked by adapton should use define/memo.

;; tests for define/memo

(module+ test
  (define/memo (add1 n) ;;return a node that adds 1 to n
    (+ 1 n))
  (define/memo (fib n) ;; return a node that computes nth number of fib
    (cond 
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (force (fib (- n 1))) (force (fib (- n 2))))]))
  
  (check-equal? (node? (add1 4)) #t)
  (check-equal? (force (add1 4)) 5)
  (check-equal? (node? (fib 4)) #t)
  (check-equal? (force (fib 4)) 3))

;; definition
(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (matt (λ (x ...) e ...))))

;; ========================================

;; a matt structure is created when a memoized function is called,
;; and invokes memo to create a thunk delaying the evaluation of 
;; that function with the given arguments.

;; tests for matt structure and memo function
(module+ test
  (define m1 (matt (λ (a) a)))
  (define m2 (matt (λ (a b) (+ a b))))
  
  ;;ensure that the nodes are well-formed
  (check-equal? (node? (m1 21)) #t)
  (check-equal? (force (m1 21)) 21)
  (check-equal? (node? (m2 21 21)) #t)
  (check-equal? (force (m2 21 21)) 42)
  
  ;;ensure they are added to the memo-table properly
  (check-equal? (node? (hash-ref *memo-table* (node-id (m1 21)))) #t)
  (check-equal? (node? (hash-ref *memo-table* (node-id (m2 21 21)))) #t))

(struct matt (f)
  #:property
  prop:procedure 
  (λ (m . xs) (apply memo m xs)))

;; calling a memoized function with args creates a node for that function
;; and those arguments, and adds that node to the memo table
(define (memo m . xs)
  (match m
    [(matt f)
     (let* ([id (equal-hash-code (cons f xs))]
           [n (node id #f '() '() (car (unbox create-stack)) xs (λ () (apply f xs)) '())]
           [t (hash-ref! *memo-table* id n)])
       (write-to-graph (format "[change]add node~n[node ~a red]~n" id))
       ; (write-to-graph (format "[change]add create edge~n[edge ~a ~a blue]~n"
       ; (car (unbox create-stack)) id))
       (if (equal? (node-args t) xs)
           t
           (hash-ref! *memo-table* 
                      (+ id 1)
                      (node (+ id 1) #f '() '() 
                            (car (unbox create-stack)) xs (λ () (apply f xs)) '()))))]))