#lang racket
;; this file contains the definition of merge-sort, 
;; as a default mergesort implementation for testing
;; and also as a function designed for adapton

(require "adapton.rkt")

(provide (all-defined-out))

;; =========================== ADAPTON MERGE-SORT ===========================

(define/memo (merge-sort l)
  (let ([fl (force l)])
    (cond
      [(empty? (force (cdr fl))) (car fl)]
      [else (force (merge-sort (force (merge-sort-helper fl))))])))

(define/memo (merge-sort-helper l)
  (cond 
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else (cons (force (merge (car l)
                              (car (force (cdr l)))))
                (force (merge-sort-helper (force (cdr (force (cdr l)))))))]))

(define/memo (merge l r)
  (cond 
    [(and (empty? l) (empty? r)) empty]
    [(empty? l) r]
    [(empty? r) l]
    [(<= (force (car l)) (force (car r)))
     (cons (car l)
           (merge (force (cdr l)) r))]
    [else 
     (cons (car r)
           (merge l (force (cdr r))))]))

;; ========================= NON-ADAPTON MERGE-SORT ==========================

(define (merge-sort-default l)
  (cond
    [(empty? (cdr l)) (car l)]
    [else (merge-sort-default (merge-sort-helper-d l))]))

(define (merge-sort-helper-d l)
  (cond 
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else (cons (merge-d (car l)
                         (car (cdr l)))
                (merge-sort-helper-d (cdr (cdr l))))]))

(define (merge-d l r)
  (cond 
    [(and (empty? l) (empty? r)) empty]
    [(empty? l) r]
    [(empty? r) l]
    [(<= (car l) (car r))
     (cons (car l)
           (merge-d (cdr l) r))]
    [else 
     (cons (car r)
           (merge-d l (cdr r)))]))
