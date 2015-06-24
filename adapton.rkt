#lang racket
(require (prefix-in r: (only-in racket delay force equal-hash-code))
         racket/format)

;; =========== GLOBAL VARIABLES ===========

;;create a top level table for nodes
(define *memo-table* 
  (make-hasheq))

;; create a top level table for cells
(define *cells*
  (make-hasheq))

;; stack 
(define stack (box '()))

;; cell counter
(define cell-counter (box 0))

;; ========================================

;; define/memo creates a memoized version of a function
(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (matt (λ (x ...) e ...))))

;; the matt structure consists of a function and its memo table
;; matt structures can be applied like functions on a list of arguments
;; "xs"
(struct matt (f)
  #:property
  prop:procedure 
  (λ (m . xs) (apply memo m xs)))

;; calling a memoized function with args creates a node for that function
;; and those arguments, and adds that node to the memo table
(define (memo m . xs)
  (match m
    [(matt f)
     (hash-ref! *memo-table* 
                (equal-hash-code (cons f xs))
                (node (equal-hash-code (cons f xs))
                      #f
                      '()
                      '()
                      (λ () (apply f xs))
                      '()))]))

;; forcing a node computes its result, keeps track of successors that it
;; forces, and handles updating of predecessors
(define (force n)
  ;; update the stack by adding this node to it
  (set-box! stack (cons (node-id n) (unbox stack)))
  ;; if there exits a node below this node on the stack, add this node to 
  ;; that node's successors and that node to this node's predecessors
  (when (not (empty? (cdr (unbox stack))))
    (node-update *memo-table* (car (cdr (unbox stack))) "successors" (node-id n))
    (node-update *memo-table* (node-id n) "predecessors" (car (cdr (unbox stack)))))
  ;; check to see if this node is already memoized, if it is use the old result
  (if (and (not (empty? (node-result (hash-ref *memo-table* (node-id n)))))
           (not (node-dirty (hash-ref *memo-table* (node-id n)))))
      (let ([result (node-result (hash-ref *memo-table* (node-id n)))])
        (set-box! stack (cdr (unbox stack)))
        result)
      ;;otherwise
      ;; compute the result of the thunk in the node
      (let ([result ((node-thunk n))])
        ;; store the result in the table
        (node-update *memo-table* (node-id n) "result" result)
        ;; remove this node from the top of the stack
        (set-box! stack (cdr (unbox stack)))
        ;; return the result
        result)))

;; ======================== DATA STRUCTURES =====================
;; node structure
(struct node (id dirty successors predecessors thunk result))

;; edge structure
(struct edge (id result))

;; cell structure
(struct cell (id box predecessors))

;; make-cell replaces boxes, allows us to assign ids to cells
;; and keep track of them in a table
(define (make-cell v)
  (set-box! cell-counter (+ 1 (unbox cell-counter)))
  (let ([c (cell (unbox cell-counter) (box v) '())])
    (printf "making cell ~a~n" (unbox cell-counter))
    (hash-ref! *cells* (unbox cell-counter) c)
    c))

;; read-cell gets the value from a cell without updating its predecessors
(define (read-cell c)
  (unbox (cell-box c)))

;; read-cell/update reads the value from a cell and updates its predecessors
(define (read-cell/update c)
  (hash-set! *cells* (cell-id c) (cell (cell-id c)
                                       (cell-box c)
                                       (cons (car (unbox stack)) (cell-predecessors c))))
  (node-update *memo-table* (car (unbox stack)) "successors" (cell-id c))
  (unbox (cell-box c)))

;; set-cell! replaces set-box!, allowing us to keep track of when
;; the user sets the value of cell
(define (set-cell! id v)
  (set-box! (cell-box (hash-ref *cells* id)) v)
  (dirty id))

;; dirty takes the predecessors of a cell and dirties all nodes
;; reachable from that cell
(define (dirty id)
  (dirty-nodes (cell-predecessors (hash-ref *cells* id))))

(define (dirty-nodes l)
  (displayln l)
  (cond
    [(empty? l) (displayln "done")]
    [else (node-update *memo-table* (car l) "dirty" #t)
          (dirty-nodes (node-predecessors (hash-ref *memo-table* (car l))))]))


;; lazy mutable lists


(define (l-cons a b)
  (cons a (λ () (make-cell b))))

(define (l-cdr l)
  (cond
    [(node? (cdr l)) (force (cdr l))]
    [(procedure? (cdr l))
     (if (cell? ((cdr l)))
         (if (empty? (read-cell ((cdr l))))
             (read-cell ((cdr l)))
             (read-cell/update ((cdr l))))
         ((cdr l)))]
    [else (cdr l)]))

;; our input should look like this
;; (make-cell (cons v (λ () (make-cell (cons v (λ () (make-cell ....
;(define example-list (make-cell (cons 3 (λ () (make-cell (cons 2 (λ () (make-cell (cons 1 (λ () empty))))))))))
;; should be the same as
;(define example-list2 (make-cell (l-cons 3 (l-cons 2 (l-cons 1 empty)))))

;; ==================== EXAMPLE FUNCTIONS =======================

(define/memo (add1 c)
  (+ 1 (read-cell/update c)))

(define/memo (add2 c)
  (+ 1 (force (add1 c))))

;(define input (make-cell 3))

;; ===========================================================
;; GRAPH VISUALIZATION
;; nodes
(define node-ids (box '()))
(define node-succs (box '()))

(define (make-nodes)
  (let ([ids
         (hash-map *memo-table* (λ (a b) a))]
        [successors
         (hash-map *memo-table* (λ (a b) (node-successors b)))])
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
       (hash-map *memo-table* (λ (a b) a))
       (hash-map *memo-table* (λ (a b) (node-dirty b))))
  (displayln "done"))

;; ========================= MERGESORT ==========================

(define input (make-cell (l-cons (l-cons 3 empty)
                                 (l-cons (l-cons 2 empty)
                                         (l-cons (l-cons 1 empty)
                                                 empty)))))

(define/memo (merge-sort l)
  (printf "merge-sort ~a~n" l)
  (cond 
    [(empty? (l-cdr l)) (car l)]
    [else (force (merge-sort (force (merge-sort-helper l))))]))

(define/memo (merge-sort-helper l)
  (printf "merge-sort-helper ~a~n" l)
  (cond 
    [(empty? l) l]
    [(empty? (l-cdr l)) l]
    [else (cons (force (merge (car l)
                              (car (l-cdr l))))
                (merge-sort-helper (l-cdr (l-cdr l))))]))

(define/memo (merge l r)
  (printf "merge ~a ~a~n" l r)
  (cond 
    [(and (empty? l) (empty? r)) empty]
    [(empty? l) r]
    [(empty? r) l]
    [(<= (car l) (car r))
     (cons (car l)
           (merge (l-cdr l) r))]
    [else 
     (cons (car r)
           (merge l (l-cdr r)))]))

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