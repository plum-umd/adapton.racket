#lang racket

(require (prefix-in r: (only-in racket delay force)))


;;create a top level table to hold a memo table for each function
(define *memo-tables* 
  (make-hasheq))

;; the matt structure consists of a function and its memo table
;; matt structures can be applied like functions on a list of arguments
;; "xs"
(struct matt (f table)
  #:property
  prop:procedure 
  (λ (m . xs) (apply memo m xs)))

;; make-memo creates a memo table for the given function
;; in the global memo table
(define (make-memo f)
  (let ((mt (hash-ref! *memo-tables* f make-hash)))
    (matt f mt)))

;; define/memo creates a memoized version of a function
(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (make-memo (λ (x ...) e ...))))

;; memoize the given function and args, 
;; then extract the thunk from the node
;; and force it. 
(define (memo m . xs)
  (force (node-thunk (apply delay m xs))))

;; given a funciton and arguments, 
;; add the arguments as a key in the hash-table for the function
;; and create a node-value for that key
(define (delay m . xs)
  (match m
    [(matt f mt)
     (hash-ref! mt xs (node (next-node-number)
                            '() 
                            (r:delay (apply f xs))))]))

;; ============================================================


;; ensures each node has a unique number id
(define node-number (box 0))
(define (next-node-number)
  (let ([out (unbox node-number)])
    (set-box! node-number (+ 1 out))
    out))

;; a node consists of an id, edges to its children, and a thunk
(struct node (id edges thunk))

;; ============================================================

;; l-cons (lazy-cons) creates a delayed list 
(define/memo (l-cons l r)
  (cons l (λ () r))) 

;; l-cdr (lazy-cdr) returns the next element of a lazy list
(define/memo (l-cdr l)
  (cond
    [(empty? l) '()]
    [(procedure? (cdr l)) ((cdr l))]
    [else (cdr l)]))


(define/memo (merge-sort l)
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