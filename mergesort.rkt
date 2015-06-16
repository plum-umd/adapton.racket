#lang racket

(require (prefix-in r: (only-in racket delay force equal-hash-code))
         racket/format)



(define name (box 1))

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
    (apply update-stack m xs)
    (let ([out (force (node-thunk (apply delay m xs)))])
      (when (not (empty? (cdr (unbox stack)))) (update-predecessors))
      out)))

;; given a funciton and arguments, 
;; add the arguments as a key in the hash-table for the function
;; and create a node-value for that key
(define (delay m . xs)
  (match m
    [(matt f mt)
     (hash-ref! *memo-tables* 
                (equal-hash-code (cons f xs))
                (node xs
                      '()
                      '()
                      (r:delay (apply f xs))))]))

;; update-stack appends the currently running node to the 
;; top of the stack
(define (update-stack m . xs)
  (match m
    [(matt f mt)
     (let ([s (unbox stack)]
           [hash (equal-hash-code (cons f xs))])
       (begin
         (set-box! stack (cons hash s))
         (cond 
           [(empty? s) (displayln "s is empty")]
           [else (update-successors (car s)
                                    hash)])))]))

;; update-successors 
(define (update-successors pred succ)
  (let ([old (hash-ref *memo-tables* pred)])
    (hash-set! *memo-tables* pred (node (node-xs old)
                                        (cons succ (node-successors old))
                                        (node-predecessors old)
                                        (node-thunk old)))))

;; update-predecessors
(define (update-predecessors)
  (let* ([succ (car (unbox stack))]
         [old (hash-ref *memo-tables* succ)])
    (set-box! stack (rest (unbox stack)))
    (let ([pred (car (unbox stack))])
      (hash-set! *memo-tables* succ (node (node-xs old)
                                          (node-successors old)
                                          (cons pred (node-predecessors old))
                                          (node-thunk old))))))

;; dirty
;; given a cell, dirty everwhere on the DCG we can reach from that cell
(define (dirty id)

;; ===========================================================
;; GRAPH VISUALIZATION

;; nodes
(define node-ids (box '()))
(define node-inputs (box '()))
(define node-succs (box '()))

(define (make-nodes)
  (let ([ids
         (hash-map *memo-tables* (λ (a b) a))]
        [successors
         (hash-map *memo-tables* (λ (a b) (node-successors b)))])
    (set-box! node-ids ids)
    (set-box! node-succs successors)))

(define (build-graph)
  (make-nodes)
  (build-graph-helper (unbox node-ids) (unbox node-succs)))

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
;; DATA STRUCTURES

;; THUNKS
;; this stack keeps track of the "currently running" thunk
;; by adding it to the front of the list. The rest of the 
;; consists of the parents of the currently running thunk.
(define stack (box empty))

;; a node consists of an id, edges to its children, and a thunk
(struct node (xs successors predecessors thunk))

;; LISTS
;; l-cons (lazy-cons) creates a delayed list 
(define/memo (l-cons l r)
  (cons l (λ () r)))

;; lm-cons (lazy. mutable-cons) creates a delayed, mutable list
(define/memo (lm-cons l r)
  (cons l (make-cell (λ () r))))

;; lm-cdr (lazy-cdr) returns the next element of a lazy 
;; (or lazy mutable) list
(define/memo (l-cdr l)
  (cond
    [(empty? l) '()]
    [(cell? (cdr l))
     (if (procedure? (read-cell (cdr l)))
         ((read-cell/update (cdr l)))
         (cdr l))]
    [else
     (if (procedure? (cdr l))
         ((cdr l))
         (cdr l))]))

;; BOXES

;; a cell is a wrapper for boxes that assigns ids to boxes
(struct cell (id box pred))

;; cell-counter contains the next box's id
(define cell-counter (box 1))

;; *cells* is a table that keeps track of all assigned cells
(define *cells* (make-hasheq))

;; make-cell replaces box, assinging an id to a box and adding
;; it to the *cells* table
(define (make-cell v)
  (let* ([id (unbox cell-counter)]
         [c (cell id (box v) '())])
    (set-box! cell-counter (+ 1 id))
    (hash-set! *cells* id c)
    c))

;; set-cell! replaces set-box!, updating the value of the box 
;; with the given id
(define (set-cell! id v)
  (set-box! (hash-ref *cells* id) v)
  (dirty id))

;; read-cell is called in place of unbox, and allows us to unbox 
;; the box contained in a cell structure
(define (read-cell b)
  (unbox (cell-box b)))

;; read-cell/update is called in place of read-cell, and allows us to 
;; set cells as children of nodes.
(define (read-cell/update b)
  (let ([old (hash-ref *memo-tables* (car (unbox stack)))])
    (hash-set! *memo-tables* 
               (car (unbox stack))
               (node (node-xs old)
                     (cons (cell-id b) (node-successors old))
                     (node-predecessors old)
                     (node-thunk old))))
  (unbox (cell-box b)))

;; ============================================================
;; MERGESORT DEFINITION

;; we use a globally defined variable to contain our input to
;; mergesort. This allows us to mutate the input later
(define input (lm-cons (lm-cons 3 empty)
                       (lm-cons (lm-cons 2 empty)
                                (lm-cons (lm-cons 1 empty)
                                         empty))))

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
     (l-cons (car l)
             (merge (l-cdr l) r))]
    [else
     (l-cons (car r)
             (merge l (l-cdr r)))]))