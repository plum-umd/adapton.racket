#lang racket

;; This file contains a number of utility functions that deal with the modification
;; of hash-table entries.

(require rackunit
         "data-structures.rkt"
         "graphing.rkt")

(provide (all-defined-out))

;; ===================================================================

;; node-update consumes a hash-table (usually *memo-table*),
;; the key of an element in the table (id of the node)
;; a field to modify in string form, and a value that the 
;; field given will be set to. This function essentially allows us to 
;; update our memo-table entries abstractly. 

(module+ test
  ;; create an example node in our hash table that our tests modify
  (hash-set! *memo-table* 
             -1 
             (node -1 #f '() '() 0 (λ () -1) #f))
  
  (check-equal? (node? (hash-ref *memo-table* -1)) #t)
  (check-equal?
   (begin (node-update *memo-table* -1 "dirty" #t)
          (node-dirty (hash-ref *memo-table* -1))) #t)
  (check-equal?
   (begin (node-update *memo-table* -1 "successors" -2)
          (node-successors (hash-ref *memo-table* -1))) '(-2))
  (check-equal?
   (begin (node-update *memo-table* -1 "predecessors" -3)
          (node-predecessors (hash-ref *memo-table* -1))) '(-3))
  (check-equal?
   (begin (node-update *memo-table* -1 "predecessors" -4)
          (node-predecessors (hash-ref *memo-table* -1))) '(-4 -3))
  (check-equal?
   (begin (node-update *memo-table* -1 "result" 1000)
          (node-result (hash-ref *memo-table* -1))) 1000))

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
      [(equal? field "result")
       (hash-set! mt id (node (node-id old-node)
                              (node-dirty old-node)
                              (node-successors old-node)
                              (node-predecessors old-node)
                              (node-creator old-node)
                              (node-thunk old-node)
                              value))])))

;; ===================================================================

;; update-predecessors takes the id of a node and the list l of that 
;; node's successors (usually). For each successor S in l, id is removed
;; from S's list of predecessors. 

;; this function is almost always called at the same time that id's 
;; list of successors is dropped, and is used to ensure that there
;; are no nodes that have id as a predecessor when id does not have
;; that node as a successor. 

;; A function is not used to drop id's successors because we can simply 
;; replace id's successors with an empty list.
;; There is a "drop-successors" function, but it is
;; only used for updating the graph that visualizes the DCG, and so
;; is found in the file "graphing.rkt".

(module+ test
  ;; create some nodes that are successors of n1
  (define n1 (node -1 
                   #f 
                   (list (edge 'n -2 0)
                         (edge 'n -3 0)
                         (edge 'n -4 0))
                   '() 
                   0 
                   (λ (x) x)
                   0))
  (define n2 (node -2 #f (list (edge 'n -3 0)) '(-1) 0 (λ (x) x) 0))
  (define n3 (node -3 #f '()                  '(-1 -2) 0 (λ (x) x) 0))
  (define n4 (node -4 #f '()                  '(-1) 0 (λ (x) x) 0))
  
  ;; add all these nodes to the memo table
  (hash-set! *memo-table* -1 n1)
  (hash-set! *memo-table* -2 n2)
  (hash-set! *memo-table* -3 n3)
  (hash-set! *memo-table* -4 n4)
  
  ;; run update-predecessors on -1
  (update-predecessors -1 (node-successors n1))
  
  (check-equal? (node-predecessors (hash-ref *memo-table* -2)) '())
  (check-equal? (node-predecessors (hash-ref *memo-table* -3)) '(-2))
  (check-equal? (node-predecessors (hash-ref *memo-table* -4)) '()))

(define (update-predecessors id l)
  (when (and (not (empty? l))
             (equal? (edge-type (car l)) 'n))
    (let ([old-n (hash-ref *memo-table* (edge-id (car l)))])
      (hash-set! *memo-table* 
                 (node-id old-n) 
                 (node (node-id old-n)
                       (node-dirty old-n)
                       (node-successors old-n)
                       (remove* (list id) (node-predecessors old-n))
                       (node-creator old-n)
                       (node-thunk old-n)
                       (node-result old-n))))
    (update-predecessors id (cdr l))))


;; ====================== CELLS ======================================
;; make-cell replaces boxes, allows us to assign ids to cells
;; and keep track of them in a table
(module+ test
  ;; make a cell
  (check-equal? (make-cell -1) (cell 1 (box -1) '() #f))
  (check-equal? 
   (unbox (cell-box (hash-ref *cells* (unbox cell-counter)))) -1))

(define (make-cell v)
  (set-box! cell-counter (+ 1 (unbox cell-counter)))
  (let ([c (cell (unbox cell-counter) (box v) '() #f)])
    (hash-ref! *cells* (unbox cell-counter) c)
    c))

;; can also use make-cell with hashing instead of counting numbers
#;(define (make-cell v)
    (let* ([id (equal-hash-code v)]
           [c (cell id (box v) '() #f)])
      (hash-ref! *cells* id c)
      c))

;; ===================================================================

;; read-cell gets the value from a cell without updating its predecessors
(define (read-cell c)
  (unbox (cell-box c)))

;; ===================================================================

;; read-cell/update reads the value from a cell and updates its predecessors
;; to reflect that it was "touched" by the node that read it. It also adds 
;; the cell to that node's successors.
(define (read-cell/update c)
  (hash-set! *cells* (cell-id c) 
             (cell (cell-id c)
                   (cell-box c)
                   (cons (car (unbox stack)) (cell-predecessors c))))
  (node-update *memo-table* (car (unbox stack)) "successors" (cell-id c))
  (unbox (cell-box c)))

;; ===================================================================

;; set-cell! replaces set-box!, allowing us to keep track of when 
;; the user sets the value of cell. It also calls the "dirty" function
;; which sets the dirty flag on every node that is a predecessor of
;; this cell (recursively)

(define (set-cell! id v)
  (set-box! (cell-box (hash-ref *cells* id)) v)
  (dirty id))

;; ====================== MODIFY NODE / CELL RELATIONSHIPS ===========

;; dirty takes the id of a cell and dirties all of that cell's predecessors.
;; for each node n that is made dirty, n's predecessors are also made dirty.

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
          (write-to-graph (format "[change]make dirty~n[node ~a green]~n" 
                                  (car l)))
          (dirty-nodes (node-predecessors (hash-ref *memo-table* (car l))))
          (dirty-nodes (cdr l))]))

;; ===================================================================

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
    
    (if (begin (write-to-graph 
                (format "[change]checking successors~n[node ~a blue]~n" 
                        (node-id n)))
               (not (andmap check-successor (node-successors n))))
        ;; if check-successor tells us that we need to recompute this node...
        (begin 
          (write-to-graph (format "[change]cleaning node~n[node ~a yellow]~n" 
                                  (node-id n)))
          (update-predecessors (node-id n) (node-successors n))
          (drop-successors (node-id n) (node-successors n))
          (hash-set! *memo-table* (node-id n) new-n)
          (let ([result (force new-n)])
            (node-update *memo-table* (node-id n) "result" result)
            (write-to-graph (format "[change]node clean~n[node ~a red]~n" 
                                    (node-id n)))
            result))
        ;; otherwise, the node's result has not changed. use old result.
        (node-result n))))

;; ===================================================================

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

;; ===================================================================

;; purge-table takes a list (usually from find-unreachable)
;; and removes unreachable nodes from the table and graph
;; --- note: this function must be called manually ---

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
               ;; --- graphing
               (write-to-graph 
                (format "[change]remove unreachable node~n[node ~a silver]~n"
                        id))
               (when graphing-on
                 (for-each 
                  (λ (x) 
                    (write-to-graph 
                     (format "[change]remove edge~n[edge ~a ~a nonactive]~n"
                             id (edge-id x))))
                  (node-successors n)))
               ;; ------------
               (for-each (λ (x) 
                           (when (and (equal? (edge-type x) 'n)
                                      (hash-ref *memo-table* (edge-id x) #f))
                             (purge-node (edge-id x) id)))
                         (node-successors n)))
        (hash-set! *memo-table* id 
                   (node id
                         (node-dirty n)
                         (node-successors n)
                         (remove* (list p) (node-predecessors n))
                         (node-creator n)
                         (node-thunk n)
                         (node-result n))))))

;; ===================================================================

;; find-unreachable looks at all the nodes in our hash table, and returns
;; the list of all nodes that are NOT reachable.

(define (find-unreachable)
  ;; let l be the list of all nodes and r be the list of all reachable nodes
  (let ([l (hash-map *memo-table* (λ (a b) a))]
        [r (hash-map *memo-table* (λ (a b) (when (reachable? b) a)))])
    (remove* r l)))

;; ===================================================================

;; reachable? returns true if a node has predecessors, or if
;; the node's creator is the root node
(define (reachable? node)
  (or (not (empty? (node-predecessors node)))
      (equal? 00000 (node-creator node))))