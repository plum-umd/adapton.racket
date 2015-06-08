#lang racket

(require (prefix-in r: (only-in racket delay force equal-hash-code)))

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
  (matt f *memo-tables*))

;; define/memo creates a memoized version of a function
(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (make-memo (λ (x ...) e ...))))

;; memoize the given function and args, 
;; then extract the thunk from the node
;; and force it. 
(define (memo m . xs)
  (begin
    (displayln xs)
    (apply update-stack m xs)
    (let ([out (force (node-thunk (apply delay m xs)))])
      (set-box! stack (rest (unbox stack)))
      out)))

;; given a funciton and arguments, 
;; add the arguments as a key in the hash-table for the function
;; and create a node-value for that key
(define (delay m . xs)
  (match m
    [(matt f mt)
     (display (equal-hash-code (cons f xs)))
     (displayln xs)
     (hash-ref! *memo-tables* 
                (equal-hash-code (cons f xs))
                (node '()
                      (r:delay (apply f xs))))]))

;; update-stack appends the currently running node to the 
;; top of the stack
(define (update-stack m . xs)
  (match m
    [(matt f mt)
     (let ([s (unbox stack)]
           [hash (equal-hash-code (cons f xs))])
       (display hash)
       (displayln xs)
       (begin
         (set-box! stack (cons (cons mt hash) s))
         (cond 
           [(empty? s) (displayln "s is empty")]
           [else (update-successors (car (car s)) 
                                    (cdr (car s)) 
                                    hash)])))]))

(define (update-successors mt pred succ)
  (let ([old (hash-ref *memo-tables* pred)])
    (hash-set! *memo-tables* pred (node (cons succ (node-edges old))
                             (node-thunk old)))))

;; ===========================================================
;; build graph

;; nodes
(define node-ids (box '()))

(define node-children (box '()))

(define (make-nodes)
  (let ([ids
         (hash-map *memo-tables* (λ (a b) a))]
        [children
         (hash-map *memo-tables* (λ (a b) (node-edges b)))])
    (set-box! node-ids ids)
    (set-box! node-children children)))

(define (build-graph)
  (make-nodes)
  (build-graph-helper (unbox node-ids) (unbox node-children)))

(define (build-graph-helper ids children)
  (cond
    [(empty? ids) empty]
    [else (cons (cons (first ids) (first children))
                (build-graph-helper (rest ids) (rest children)))]))

(define (e l)
  (cond 
    [(empty? l) l]
    [else (append (first l) (e (rest l)))]))


(define (port-to-graphmovie l)
  (cond
    [(empty? l) (displayln "done")]
    [else (begin (printf "[node ~a red]~n" (car (car l)))
                 (for-each (λ (a) (printf "[edge ~a ~a]~n" 
                                          (car (car l))
                                          a)) (cdr (car l)))
                 (port-to-graphmovie (cdr l)))]))

;; ============================================================

;; this stack keeps track of the "currently running" thunk
;; by adding it to the front of the list. The rest of the 
;; consists of the parents of the currently running thunk.
(define stack (box empty))

;; a node consists of an id, edges to its children, and a thunk
(struct node (edges thunk))

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