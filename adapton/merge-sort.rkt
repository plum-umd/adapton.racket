#lang racket
;; this file contains the definition of merge-sort, 
;; as a default mergesort implementation for testing
;; and also as a function designed for adapton

(require rackunit
         "memo-table-modification-tools.rkt"
         "data-structures.rkt"
         "adapton.rkt")

(provide (all-defined-out))

;; ========================= NON-ADAPTON MERGE-SORT ==========================

;; This definition of merge-sort attempts to closely follow the algorithm of
;; merge-sort for adapton. This definition is here for testing.

;; merge-sort-default consumes a list l of singleton lists. If there is only
;; one element in l, it returns that element. Otherwise, it calls itself
;; recursively with the output of merge-sort-helper as its input.
;; merge-sort-helper will always return a smaller list than it is given.

(module+ test
  (check-equal? (merge-sort-default '((3) (2) (1)))
                '(1 2 3)))

(define (merge-sort-default l)
  (cond
    [(empty? (cdr l)) (car l)]
    [else (merge-sort-default (merge-sort-helper-d l))]))

;; merge-sort-helper gets a list l of singleton lists, and merges the first
;; two lists in l, cons-ing the result onto its recursive call. 

(module+ test
  (check-equal? (merge-sort-helper-d '((3) (2) (1)))
                '((2 3) (1)))
  (check-equal? (merge-sort-helper-d '((1)))
                '((1)))
  (check-equal? (merge-sort-helper-d
                 '((7) (6) (5) (4) (3) (2) (1)))
                '((6 7) (4 5) (2 3) (1)))
  (check-equal? (merge-sort-helper-d 
                 (merge-sort-helper-d
                  '((7) (6) (5) (4) (3) (2) (1))))
                '((4 5 6 7) (1 2 3))))

(define (merge-sort-helper-d l)
  (cond 
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else (cons (merge-d (car l)
                         (car (cdr l)))
                (merge-sort-helper-d (cdr (cdr l))))]))

;; merge consumes two sorted lists and merges them

(module+ test
  (check-equal? (merge-d '(3) '(2))
                '(2 3))
  (check-equal? (merge-d '(1 2 3 4) '(5 6 7 8))
                '(1 2 3 4 5 6 7 8))
  (check-equal? (merge-d '(1 5 7 9) '(2 4 6 8))
                '(1 2 4 5 6 7 8 9))
  (check-equal? (merge-d '(1 5 3 2) '(2 9 3 5)) ;; input lists will always
                '(1 2 5 3 2 9 3 5)))            ;; be sorted, so this will 
;; never occur

(define (merge-d l r)
  (cond 
    [(and (empty? l) (empty? r)) empty]
    [(empty? l) r]
    [(empty? r) l]
    [(< (car l) (car r))
     (cons (car l)
           (merge-d (cdr l) r))]
    [(= (car l) (car r))
     (cons (car l)
           (cons (car r) (merge-d (cdr l) (cdr r))))]
    [else 
     (cons (car r)
           (merge-d l (cdr r)))]))

;; =========================== ADAPTON MERGE-SORT ===========================

;; merge-sort defined for adapton works EXACTLY as above, but all functions
;; return delayed computations (nodes) and all inputs are wrapped in cells.
;; the force function is used to "unwrap" values from the formatted input

;; These functions use define/memo, and all return nodes.


;; merge-sort consumes a list l of singleton lists. If there is only
;; one element in l, it returns that element. Otherwise, it calls itself
;; recursively with the output of merge-sort-helper as its input.
;; merge-sort-helper will always return a smaller list than it is given.
(module+ test
  (define l (make-cell 
             (cons (cons (make-cell 9) empty)
                   (make-cell
                    (cons (cons (make-cell 6) empty)
                          (make-cell 
                           (cons (cons (make-cell 3) empty)
                                 empty)))))))
  
  (check-equal? (node? (merge-sort l))
                #t)
  (let ([r (force (merge-sort l))])
    (check-equal? (and (pair? r)
                       (cell? (car (force r))))
                  #t))
  (check-equal? (force (car (force (merge-sort l))))
                3))

;; definitions
(define/memo (merge-sort l)
  (let ([fl (force l)])
    (cond
      [(empty? (force (cdr fl))) (car fl)]
      [else (force (merge-sort (force (merge-sort-helper fl))))])))

(define/memo (merge-sort-helper l)
  (cond 
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else
     (cons (force (merge (car l)
                         (car (force (cdr l)))))
           (force (merge-sort-helper (force (cdr (force (cdr l)))))))]))

(define/memo (merge l r)
  (cond 
    [(and (empty? l) (empty? r)) empty]
    [(empty? l) r]
    [(empty? r) l]
    [(< (force (car l)) (force (car r)))
     (cons (car l)
           (merge (force (cdr l)) r))]
    [(= (force (car l)) (force (car r)))
     (cons (car l)
           (n-cons (car r)
                   (merge (force (cdr l)) (force (cdr r)))))]
    [else 
     (cons (car r)
           (merge l (force (cdr r))))]))

(define/memo (n-cons l r)
  (cons l r))
