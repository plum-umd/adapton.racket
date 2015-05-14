#lang racket

(require (prefix-in r: (only-in racket delay force)))

(define *memo-tables* 
  (make-hasheq))

(struct matt (f table)
  #:property
  prop:procedure 
  (λ (m . xs) (apply memo m xs)))

(define (make-memo f)
  (let ((mt (hash-ref! *memo-tables* f make-hash)))
    (matt f mt)))

(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (make-memo (λ (x ...) e ...))))

(define (memo m . xs)
  (force (apply delay m xs)))

(define (delay m . xs)
  (match m
    [(matt f mt)
     (hash-ref! mt xs (λ () (r:delay (apply f xs))))]))

;; ============================================================

(struct node (id edges thunk))

;; ============================================================


(define/memo (l-cons l r)
  (cons l (λ () r))) 

(define/memo (l-cdr l)
  (cond
    [(empty? l) '()]
    [(procedure? (cdr l)) ((cdr l))]
    [else (cdr l)]))


(define/memo (merge-sort l)
  ;(displayln l)
  (cond
    [(empty? (l-cdr l)) (car l)]
    [else (merge-sort (merge-sort-helper l))]))

(define/memo (merge-sort-helper l)
  (cond
    [(empty? (l-cdr l)) l]
    [(l-cons (merge (car l) 
                    (car (l-cdr l)))
             (merge-sort-helper (l-cdr (l-cdr l))))]))

(define/memo (merge l r)
  (display l)
  (displayln r)
  (cond
    [(and (empty? l) (empty? r)) '()]
    [(empty? l) r]
    [(empty? r) l]
    [(<= (car l) (car r))
     (cons (car l)
           (λ () (merge (l-cdr l) r)))]
    [else
     (cons (car r)
           (λ () (merge l (l-cdr r))))]))