#lang racket
(require (prefix-in r: (only-in racket delay force equal-hash-code))
         racket/format)

;; ============= GRAPH ===============

(displayln (current-directory))

(define file (string-append (~a (current-directory)) "graph.gmv"))
(display-to-file (format "[styleselect colors]~n")
                 file
                 #:mode 'text
                 #:exists 'replace)

(define (write-to-graph string)
  (display-to-file string	 
                   file
                   #:mode 'text
                   #:exists 'append))

(write-to-graph (format "[state]make red~n"))

;; drop successors takes a node and updates that node's edges on the graph
;; drop successors is called when a node is cleaned and must be re-evaluated,
;; at which point its list of successors is dropped. This function simply
;; updates our graph to reflect the new edges.
(define (drop-successors id l)
  (cond
    [(empty? l) "all edges removes"]
    [else (write-to-graph (format "[change]remove edge~n[edge ~a ~a nonactive]~n" 
                                  id 
                                  (edge-id (car l))))
          (drop-successors id (cdr l))]))

;; remove nodes takes a node and removes it from the graph. Then,
;; it removes that node's successors recursively if those successors
;; had only one predecessor
(define (remove-nodes n)
  (when (and (node? n)
             (<= (length (node-predecessors n)) 1))
    (write-to-graph (format "[change]remove node~n[node ~a nonactive]~n" (node-id n)))
    (
    

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
     (let ([id (equal-hash-code (cons f xs))])
       (write-to-graph (format "[change]add node~n[node ~a red]~n" id))
       (hash-ref! *memo-table* 
                  (equal-hash-code (cons f xs))
                  (node (equal-hash-code (cons f xs))
                        #f
                        '()
                        '()
                        (λ () (apply f xs))
                        '())))]))

;; forcing an articulation point computes its result.
;; for nodes, the result is computer and stored in the node
;; structure. For cells, the result is unboxed. Force also
;; keeps track of successors and predecessors
(define (force a)
  (cond 
    [(node? a)
     ;; update the stack by adding this node to it
     (set-box! stack (cons (node-id a) (unbox stack)))
     ;; is this node dirty?
     (if (node-dirty (hash-ref *memo-table* (node-id a)))
         ;; if yes, clean it and return the cleaned result
         (begin (set-box! stack (cdr (unbox stack)))
                (let ([result (clean a)])
                  result))
         ;; otherwise
         ;; check to see if this node is already memoized, if it is use the old result
         (if (not (empty? (node-result (hash-ref *memo-table* (node-id a)))))
             (let ([result (node-result (hash-ref *memo-table* (node-id a)))])
               ;; if there exits a node below this node on the stack, add this node to 
               ;; that node's successors and that node to this node's predecessors 
               (when (not (empty? (cdr (unbox stack))))
                 (node-update *memo-table* (car (cdr (unbox stack))) "successors" (edge 'n
                                                                                        (node-id a)
                                                                                        result))
                 (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" (car (cdr (unbox stack))) (node-id a)))
                 (node-update *memo-table* (node-id a) "predecessors" (car (cdr (unbox stack)))))
               (set-box! stack (cdr (unbox stack)))
               result)
             ;; otherwise
             ;; compute the result of the thunk in the node
             (let ([result ((node-thunk a))])
               ;; store the result in the table
               (node-update *memo-table* (node-id a) "result" result)
               ;; if there exits a node below this node on the stack, add this node to 
               ;; that node's successors and that node to this node's predecessors
               (when (not (empty? (cdr (unbox stack))))
                 (node-update *memo-table* (car (cdr (unbox stack))) "successors" (edge 'n
                                                                                        (node-id a)
                                                                                        result))
                 (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" (car (cdr (unbox stack))) (node-id a)))
                 (node-update *memo-table* (node-id a) "predecessors" (car (cdr (unbox stack)))))
               ;; remove this node from the top of the stack
               (set-box! stack (cdr (unbox stack)))
               ;; return the result
               result)))]
    [(cell? a) 
     ;; update the stack by adding this cell to it
     (set-box! stack (cons (cell-id a) (unbox stack)))
     ;; if there exits a node below this cell on the stack, add this cell to 
     ;; that node's successors and that node to this cell's predecessors
     (when (and (not (empty? (unbox stack)))
                (not (empty? (cdr (unbox stack)))))
       (node-update *memo-table* (car (cdr (unbox stack))) "successors" (edge 'c
                                                                              (cell-id a) 
                                                                              '()))
       (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" (car (cdr (unbox stack))) (cell-id a)))
       (let ([old-cell (hash-ref *cells* (cell-id a))])
         (hash-set! *cells* (cell-id a) (cell (cell-id a)
                                              (cell-box old-cell)
                                              (cons (car (cdr (unbox stack))) 
                                                    (cell-predecessors old-cell))
                                              (cell-dirty old-cell)))))
     ;; extract the value of this cell
     (let ([result (unbox (cell-box a))])
       (set-box! stack (cdr (unbox stack)))
       result)]
    [else a]))

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
  (displayln l)
  (cond
    [(empty? l) (displayln "done")]
    [(node-dirty (hash-ref *memo-table* (car l))) (displayln "done")]
    [else (node-update *memo-table* (car l) "dirty" #t)
          (write-to-graph (format "[change]make dirty~n[node ~a green]~n" (car l)))
          (dirty-nodes (node-predecessors (hash-ref *memo-table* (car l))))
          (dirty-nodes (cdr l))]))

;; clean takes a node and does the minimum amount of work necessary to decide
;; whether or not a dirty node needs to be recomputed. If it does, clean will 
;; recompute that node and drop its successors
(define (clean n)
  (printf "cleaning ~a~n" (node-id n))
  (let ([new-n (node (node-id n)
                     #f
                     '()
                     (node-predecessors n)
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

;; ======================== DATA STRUCTURES =====================
;; node structure
(struct node (id dirty successors predecessors thunk result))

;; USER NODE
;; serves as root node for all user forced nodes
(hash-set! *memo-table* 00000 (node 00000 #f '() '() (λ () (displayln "forcing root node")) 00000))
(write-to-graph (format "[change]add node~n[node ~a red]~n" 00000))

;; edge structure
;; type is one of 'n (node) or 'c (cell),
;; id is the target of the edge
;; result is the this edge's knowledge of the old result
;; of its target.
(struct edge (type id result))

;; cell structure
(struct cell (id box predecessors dirty))

;; make-cell replaces boxes, allows us to assign ids to cells
;; and keep track of them in a table
(define (make-cell v)
  (set-box! cell-counter (+ 1 (unbox cell-counter)))
  (let ([c (cell (unbox cell-counter) (box v) '() #f)])
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

;; ========================= MERGESORT ========================== 

#;(define input (make-cell (cons (cons 3 empty)
                                 (make-cell (cons (cons 2 empty)
                                                  (make-cell (cons (cons 1 empty)
                                                                   empty)))))))

(define input (make-cell 
               (cons (cons (make-cell 3) empty)
                     (make-cell 
                      (cons (cons (make-cell 6) empty)
                            (make-cell 
                             (cons (cons (make-cell 9) empty)
                                   (make-cell 
                                    (cons (cons (make-cell 2) empty)
                                          (make-cell 
                                           (cons (cons (make-cell 4) empty)
                                                 (make-cell 
                                                  (cons (cons (make-cell 5) empty)
                                                        (make-cell 
                                                         (cons (cons (make-cell 8) empty)
                                                               (make-cell 
                                                                (cons (cons (make-cell 1) empty)
                                                                      empty)))))))))))))))))
(define/memo (merge-sort l)
  (printf "merge-sort ~a~n" l)
  (let ([fl (force l)])
    (printf "merge-sort ~a~n" fl)
    (cond
      [(empty? (force (cdr fl))) (car fl)]
      [else (force (merge-sort (force (merge-sort-helper fl))))])))

(define/memo (merge-sort-helper l)
  (printf "merge-sort-helper ~a~n" l)
  (cond 
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else (cons (force (merge (car l)
                              (car (force (cdr l)))))
                (force (merge-sort-helper (force (cdr (force (cdr l)))))))]))

(define/memo (merge l r)
  (printf "merge ~a ~a~n" l r)
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

(define (update-predecessors id l)
  (when (and (not (empty? l))
             (equal? (edge-type (car l)) 'n))
    (let ([old-n (hash-ref *memo-table* (edge-id (car l)))])
      (hash-set! *memo-table* (node-id old-n) (node (node-id old-n)
                                                    (node-dirty old-n)
                                                    (node-successors old-n)
                                                    (remove* (list id) (node-predecessors old-n))
                                                    (node-thunk old-n)
                                                    (node-result old-n))))
    (update-predecessors id (cdr l))))