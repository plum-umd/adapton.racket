#lang racket


;; This file contains a number of utility functions that deal with the modification
;; of hash-table entries. Specifically, these functions allow you to update an entry 
;; in a table of NODES (memo-table) with fewer lines of code. 

;; ============================== NODES ===============================

(require "data-structures.rkt"
         "graphing.rkt")

(provide (all-defined-out))

(define (node-update mt id field value)
  (let ([old-node (hash-ref mt id)])
    (cond
      [(equal? field "dirty") 
       (hash-set! mt id (node (node-id old-node)
                              value
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-creator old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "successors")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (cons value (node-successors old-node))
                              (node-predecessors old-node)
                              (node-creator old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "predecessors")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (node-successors old-node)
                              (cons value (node-predecessors old-node))
                              (node-creator old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "dirty")
       (hash-set! mt id (node (node-id old-node)
                              value
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-creator old-node)
                              (node-thunk old-node)
                              (node-result old-node)))]
      [(equal? field "result")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-creator old-node)
                              (node-thunk old-node)
                              value))])))

(define (node-update-creator mt id value)
  (let ([old-node (hash-ref mt id)])
    (hash-set! mt id (node (node-id old-node)
                          (node-dirty old-node)
                          (node-successors old-node)
                          (node-predecessors old-node)
                          value
                          (node-thunk old-node)
                          (node-result old-node)))))

(define (node-clear-successors mt id)
  (let ([old-node (hash-ref mt id)])
    (hash-set! mt id (node (node-id old-node)
                           (node-dirty old-node)
                           '()
                           (node-predecessors old-node)
                           (node-creator old-node)
                           (node-thunk old-node)
                           (node-result old-node)))))

(define (node-clear-predecessors mt id)
  (let ([old-node (hash-ref mt id)])
    (hash-set! mt id (node (node-id old-node)
                           (node-dirty old-node)
                           (node-successors old-node)
                           '()
                           (node-creator old-node)
                           (node-thunk old-node)
                           (node-result old-node)))))

(define (update-predecessors id l)
  (when (and (not (empty? l))
             (equal? (edge-type (car l)) 'n))
    (let ([old-n (hash-ref *memo-table* (edge-id (car l)))])
      (hash-set! *memo-table* (node-id old-n) (node (node-id old-n)
                                                    (node-dirty old-n)
                                                    (node-successors old-n)
                                                    (remove* (list id) (node-predecessors old-n))
                                                    (node-creator old-n)
                                                    (node-thunk old-n)
                                                    (node-result old-n))))
    (update-predecessors id (cdr l))))


;; ====================== CELLS ==========================
;; make-cell replaces boxes, allows us to assign ids to cells
;; and keep track of them in a table
(define (make-cell v)
  (set-box! cell-counter (+ 1 (unbox cell-counter)))
  (let ([c (cell (unbox cell-counter) (box v) '() #f)])
    (hash-ref! *cells* (unbox cell-counter) c)
    c))

#;(define (make-cell v)
  (let* ([id (equal-hash-code v)]
         [c (cell id (box v) '() #f)])
    (hash-ref! *cells* id c)
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

;; ====================== MODIFY NODE / CELL RELATIONSHIPS =====================
;; dirty takes the predecessors of a cell and dirties all nodes
;; reachable from that cell
(define (dirty id)
  (write-to-graph (format "[change]make dirty~n[node ~a green]~n" id))
  (let ([old-cell (hash-ref *cells* id)])
    (hash-set! *cells* id (cell id
                                (cell-box old-cell)
                                (cell-predecessors old-cell)
                                #t))
    (dirty-nodes (cell-predecessors old-cell))))

(define (dirty-nodes l)
  (cond
    [(empty? l) "done"]
    [(node-dirty (hash-ref *memo-table* (car l))) "done"]
    [else (node-update *memo-table* (car l) "dirty" #t)
          (write-to-graph (format "[change]make dirty~n[node ~a green]~n" (car l)))
          (dirty-nodes (node-predecessors (hash-ref *memo-table* (car l))))
          (dirty-nodes (cdr l))]))

;; clean takes a node and does the minimum amount of work necessary to decide
;; whether or not a dirty node needs to be recomputed. If it does, clean will 
;; recompute that node and drop its successors
(define (clean n)
  (let ([new-n (node (node-id n)
                     #f
                     '()
                     (node-predecessors n)
                     0
                     (node-thunk n)
                     '())])
    
    (if (begin (write-to-graph (format "[change]checking successors~n[node ~a blue]~n" (node-id n)))
               (not (andmap check-successor (node-successors n))))
        (begin 
          (write-to-graph (format "[change]cleaning node~n[node ~a yellow]~n" (node-id n)))
          (update-predecessors (node-id n) (node-successors n))
          (drop-successors (node-id n) (node-successors n))
          (hash-set! *memo-table* (node-id n) new-n)
          (let ([result (force new-n)])
            (node-update *memo-table* (node-id n) "result" result)
            (write-to-graph (format "[change]node clean~n[node ~a red]~n" (node-id n)))
            result))
        (node-result n))))

;; check-successors takes a node's list of successors and checks to see if they
;; are dirty. it will clean the first dirty successor that it finds, and compare
;; the new result to the old result. If the results are different than the original 
;; node needs to be recomputed, and we return simply #f, indicating to the clean 
;; function that the original node in question needs to be recomputed. 
(define (check-successor e)
  (cond
    [(and (equal? (edge-type e) 'c)
          (cell-dirty (hash-ref *cells* (edge-id e))))
     #f]
    [(and (equal? (edge-type e) 'n)
          (node-dirty (hash-ref *memo-table* (edge-id e))))
     (begin (clean (hash-ref *memo-table* (edge-id e)))
            (if (equal? (edge-result e)
                        (node-result (hash-ref *memo-table* (edge-id e))))
                #t
                #f))]
    [(equal? (edge-type e) 'n)
     (if (equal? (edge-result e)
                 (node-result (hash-ref *memo-table* (edge-id e))))
         #t
         #f)]))

;; purge-table takes a list (usually from find-unreachable)
;; and removes unreachable nodes from the table and graph
(define (purge-unreachable l)
  (cond
    [(empty? l) "done"]
    [else (begin 
            (purge-node (car l) '())
            (purge-unreachable (cdr l)))]))

(define (purge-node id p)
  (let ([n (hash-ref *memo-table* id)])
    (if (>= 1 (length (node-predecessors n)))
        (begin (hash-remove! *memo-table* id)
               (write-to-graph (format "[change]remove unreachable node~n[node ~a silver]~n" id))
               (when graphing-on
                 (for-each 
                  (λ (x) (write-to-graph (format "[change]remove edge~n[edge ~a ~a nonactive]~n" 
                                                 id (edge-id x))))
                                         (node-successors n)))
               (for-each (λ (x) (when (and (equal? (edge-type x) 'n)
                                           (hash-ref *memo-table* (edge-id x) #f))
                                  (purge-node (edge-id x) id)))
                         (node-successors n)))
        (hash-set! *memo-table* id (node id
                                         (node-dirty n)
                                         (node-successors n)
                                         (remove* (list p) (node-predecessors n))
                                         (node-creator n)
                                         (node-thunk n)
                                         (node-result n))))))

(define (find-unreachable)
  ;; let l be the list of all nodes and r be the list of all reachable nodes
  (let ([l (hash-map *memo-table* (λ (a b) a))]
        [r (hash-map *memo-table* (λ (a b) (when (reachable? b) a)))])
    (remove* r l)))

;; reachable? returns true if a node has predecessors, or if
;; the node's creator is the root node
(define (reachable? node)
  (or (not (empty? (node-predecessors node)))
      (equal? 00000 (node-creator node))))