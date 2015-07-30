#lang racket
;; This file contains a number of useful tools for the racket adapton implementation

(require "merge-sort.rkt"
         "memo-table-modification-tools.rkt"
         "data-structures.rkt"
         "adapton.rkt")

(provide (all-defined-out))


;; ================================= TESTING =================================
;; Tools for testing

(define (get-list-from-mergesort a)
  (map force (get-unforced-list-from-mergesort a)))

(define (get-unforced-list-from-mergesort a)
  (cond
    [(empty? (cdr a)) (cons (car a) empty)]
    [else (cons (car a)
                (get-unforced-list-from-mergesort (force (cdr a))))]))

(define (print-list-from-delayed-list l)
  (cond
    [(empty? l) empty]
    [else (cons (list (force (car (car (force l)))))
                (print-list-from-delayed-list (cdr (force l))))]))

(define (m-cons l r)
  (make-cell (cons (cons (make-cell l) empty)
                   r)))

(define (build-input n)
  (cond 
    [(< n 1) empty]
    [else (m-cons n (build-input (- n 1)))]))

(define (build-trivial-input n)
  (cond
  [(< n 1) empty]
  [else (m-cons 1 (build-trivial-input (- n 1)))]))

(define (build-sorted-input n m)
  (cond
    [(> n m) empty]
    [else (m-cons n (build-sorted-input (+ n 1) m))]))

(define (build-trivial-list n)
  (cond
    [(< n 1) empty]
    [else (cons (cons 1 empty) (build-trivial-list (- n 1)))]))
(define (build-list n)
  (cond
    [(< n 1) empty]
    [else (cons (cons n empty) (build-list (- n 1)))]))