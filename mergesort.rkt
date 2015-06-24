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
  (apply delay m xs))

(define (force n)
  (displayln stack)
  (update-stack n)
  (let* ([result (r:force (node-thunk n))]
         [old-node-id (car (unbox stack))]
         [old-node (hash-ref *memo-tables* old-node-id)])
    (node-update *memo-tables* old-node-id "result" result)
    (when (not (empty? (cdr (unbox stack)))) (update-predecessors))
    result))

;; given a funciton and arguments, 
;; add the arguments as a key in the hash-table for the function
;; and create a node-value for that key
(define (delay m . xs)
  (match m
    [(matt f mt)
     (hash-ref! *memo-tables* 
                (equal-hash-code (cons f xs))
                (node (equal-hash-code (cons f xs))
                      #f
                      '()
                      '()
                      (r:delay (apply f xs))
                      '()))]))

;; update-stack appends the currently running node to the 
;; top of the stack
(define (update-stack n)
  (let ([s (unbox stack)])
    (begin
      (set-box! stack (cons (node-id n) s))
      (cond 
        [(empty? s) (displayln "s is empty")]
        [else (update-successors (car s)
                                 (node-id n))]))))

;; update-successors 
(define (update-successors pred succ)
  (node-update *memo-tables* pred "successors" succ))

;; update-predecessors
(define (update-predecessors)
  (let* ([succ (car (unbox stack))])
    (set-box! stack (rest (unbox stack)))
    (let ([pred (car (unbox stack))])
      (node-update *memo-tables* succ "predecessors" pred))))

;; dirty
;; dirty-nodes contains a list of all the dirty nodes in the dcg
(define dirty-nodes (box '()))

;; given a cell, dirty everwhere on the DCG we can reach from that cell
(define (dirty id)
  (dirty-cell-children (cell-predecessors (hash-ref *cells* id))))

(define (dirty-cell-children l)
  (cond 
    [(empty? l) (displayln "done")]
    [(node-dirty (hash-ref *memo-tables* (car l))) (displayln "done")]
    [else (let ([old-node (hash-ref *memo-tables* (car l))])
            (begin 
              (node-update *memo-tables* (car l) "dirty" #t)
              (dirty-node-children (node-predecessors old-node))
              (dirty-cell-children (cdr l))))]))

(define (dirty-node-children l)
  (cond
    [(empty? l) (displayln "done")]
    [(node-dirty (hash-ref *memo-tables* (car l))) (displayln "done")]
    [else (let ([old-node (hash-ref *memo-tables* (car l))])
            (begin 
              (node-update *memo-tables* (car l) "dirty" #t)
              (dirty-node-children (node-predecessors (hash-ref *memo-tables* (car l))))
              (dirty-node-children (cdr l))))]))

;; ===========================================================
;; GRAPH VISUALIZATION

;; nodes
(define node-ids (box '()))
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

(define (port-to-graphmovie l)
  (cond
    [(empty? l) (displayln "done")]
    [else (begin (printf "[node ~a red]~n" (car (car l)))
                 (for-each (λ (a) (printf "[edge ~a ~a]~n" 
                                          (car (car l))
                                          a)) (cdr (car l)))
                 (port-to-graphmovie (cdr l)))]))

(define (dirty-graph color)
  (printf "[change] make-dirty ~a~n" color)
  (map (λ (id dirty) (when dirty (printf "[node ~a ~a]~n" id color)))
       (hash-map *memo-tables* (λ (a b) a))
       (hash-map *memo-tables* (λ (a b) (node-dirty b))))
  (displayln "done"))


;; ============================================================
;; DATA STRUCTURES

;; THUNKS
;; this stack keeps track of the "currently running" thunk
;; by adding it to the front of the list. The rest of the 
;; consists of the parents of the currently running thunk.
(define stack (box empty))

;; a node consists of an id, edges to its children, and a thunk
(struct node (id dirty successors predecessors thunk result))

;; LISTS
;; lm-cons (lazy. mutable-cons) creates a delayed, mutable list
(define (lm-cons l r)
  (cons l (make-cell (λ () r))))

;; l-cdr (lazy-cdr) returns the next element of a lazy 
;; (or lazy mutable) list
(define (l-cdr l)
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

;; MUTABLE REFERENCE-CELLS 
;; a cell is a wrapper for boxes that assigns ids to boxes
(struct cell (id box predecessors))

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
  (set-box! (cell-box (hash-ref *cells* id)) v)
  (dirty id))

;; read-cell is called in place of unbox, and allows us to unbox 
;; the box contained in a cell structure
(define (read-cell b)
  (unbox (cell-box b)))

;; read-cell/update is called in place of read-cell, and allows us to 
;; set cells as children of nodes.
(define (read-cell/update b)
  (displayln (cell-id b))
  (node-update *memo-tables* (car (unbox stack)) "successors" (cell-id b))
  (let ([old-cell (hash-ref *cells* (cell-id b))])
    (hash-set! *cells* (cell-id b) (cell (cell-id old-cell)
                                         (cell-box old-cell)
                                         (cons (car (unbox stack)) (cell-predecessors old-cell))))
    (unbox (cell-box b))))

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
    [else (force (merge-sort (force (merge-sort-helper l))))]))

(define/memo (merge-sort-helper l)
  (cond
    [(empty? (l-cdr l)) l]
    [(cons (force (merge (car l) 
                         (car (l-cdr l))))
           (force (merge-sort-helper (l-cdr (l-cdr l)))))]))

(define/memo (merge l r)
  (printf "merge ~a ~a~n" l r)
  (cond 
    [(and (empty? l) (empty? r)) '()]
    [(empty? l) r]
    [(empty? r) l] 
    [(<= (car l) (car r)) 
     (cons (car l)
           (λ () (force (merge (l-cdr l) r))))]
    [else
     (cons (car r)
           (λ () (force (merge l (l-cdr r)))))]))


;; ==============================================================
;; TO BE MOVED
(define (node-update mt id field value)
  (let ([old-node (hash-ref mt id)])
    (cond
      [(equal? field "dirty") 
       (hash-set! mt id (node (node-id old-node)
                              value
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "successors")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (cons value (node-successors old-node))
                              (node-predecessors old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "predecessors")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (node-successors old-node)
                              (cons value (node-predecessors old-node))
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "result")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-thunk old-node)
                              value))])))

(define (node-clear-successors mt id)
  (let ([old-node (hash-ref mt id)])
    (hash-set! mt id (node (node-id old-node)
                           (node-dirty old-node)
                           '()
                           (node-predecessors old-node)
                           (node-thunk old-node)
                           (node-result old-node)))))

(define (node-clear-predecessors mt id)
  (let ([old-node (hash-ref mt id)])
    (hash-set! mt id (node (node-id old-node)
                           (node-dirty old-node)
                           (node-successors old-node)
                           '()
                           (node-thunk old-node)
                           (node-result old-node)))))