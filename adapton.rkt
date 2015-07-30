#lang racket

;; This file contains the core of adapton, consisting of:
;; - definition of functions memoized for adapton
;; - creation of nodes when those functions are called
;; - force for nodes and cells

(require (prefix-in r: (only-in racket delay force equal-hash-code))
         racket/format
         rackunit
         rackunit/text-ui
         "memo-table-modification-tools.rkt"
         "graphing.rkt"
         "data-structures.rkt")

(provide (all-defined-out))

;; ========================================

;; define/memo creates a memoized version of a function
;; any function that you want to be tracked by adapton should use define/memo.

;; tests for define/memo
#;
(module+ test
  (define/memo (add1 n) ;;return a node that adds 1 to n
    (+ 1 n))
  (define/memo (fib n) ;; return a node that computes nth number of fib
    (cond 
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (force (fib (- n 1))) (force (fib (- n 2))))]))
  
  (check-equal? (node? (add1 4)) #t)
  (check-equal? (force (add1 4)) 5)
  (check-equal? (node? (fib 4)) #t)
  (check-equal? (force (fib 4)) 3))

;; definition
(define-syntax-rule
  (define/memo (f x ...) e ...)
  (define f
    (matt (λ (x ...) e ...))))

;; ========================================

;; a matt structure is created when a memoized function is called,
;; and invokes memo to create a thunk delaying the evaluation of 
;; that function with the given arguments.

;; tests for matt structure and memo function
(module+ test
  (define m1 (matt (λ (a) a)))
  (define m2 (matt (λ (a b) (+ a b))))
  
  ;;ensure that the nodes are well-formed
  (check-equal? (node? (m1 21)) #t)
  (check-equal? (force (m1 21)) 21)
  (check-equal? (node? (m2 21 21)) #t)
  (check-equal? (force (m2 21 21)) 42)
  
  ;;ensure they are added to the memo-table properly
  (check-equal? (node? (hash-ref *memo-table* (node-id (m1 21)))) #t)
  (check-equal? (node? (hash-ref *memo-table* (node-id (m2 21 21)))) #t))

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
       ; (write-to-graph (format "[change]add create edge~n[edge ~a ~a blue]~n"
       ; (car (unbox create-stack)) id))
       (hash-ref! *memo-table* 
                  (equal-hash-code (cons f xs))
                  (node (equal-hash-code (cons f xs))
                        #f
                        '()
                        '()
                        (car (unbox create-stack))
                        (λ () (apply f xs))
                        '())))]))


;; An articulation point is one of 
;;    | node
;;    | cell

;; forcing an articulation point computes its result.
;; for nodes, the result is computed and stored in the node structure.
;; For cells, the result is the value contained in the cell's box.

;; Force also keeps track of the relationships between nodes and cells,
;; building the DCG each time an articulation point is forced. We use
;; a stack to keep track of which AP is currently being forced, and 
;; leverage the information stored in that stack to update predecessors
;; and successors for each AP.
(define (force a)
  (cond
    [(node? a)
     ;; update the stack by adding this node to it
     (set-box! stack (cons (node-id a) (unbox stack)))
     (set-box! create-stack (cons (node-id a) (unbox create-stack)))
     ;; is this node dirty?
     (if (node-dirty (hash-ref *memo-table* (node-id a)))
         ;; if yes, clean it and return the cleaned result
         (begin (set-box! stack (cdr (unbox stack)))
                (set-box! create-stack (cdr (unbox create-stack)))
                (let ([result (clean a)])
                  result))
         ;; otherwise
         ;; check to see if this node is already memoized, if it is use the old result
         (if (not (empty? (node-result (hash-ref *memo-table* (node-id a)))))
             (let ([result (node-result (hash-ref *memo-table* (node-id a)))])
               ;; if there exits a node below this node on the stack, add this node to 
               ;; that node's successors and that node to this node's predecessors 
               (when (not (empty? (cdr (unbox stack))))
                 (node-update *memo-table* 
                              (car (cdr (unbox stack))) 
                              "successors" 
                              (edge 'n
                                    (node-id a)
                                    result))
                 (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" 
                                         (car (cdr (unbox stack))) 
                                         (node-id a)))
                 (node-update *memo-table* (node-id a) 
                              "predecessors" (car (cdr (unbox stack))))) 
               (set-box! stack (cdr (unbox stack)))
               (set-box! create-stack (cdr (unbox create-stack)))
               result)
             ;; otherwise
             ;; compute the result of the thunk in the node
             (let ([result ((node-thunk a))])
               ;; store the result in the table
               (node-update *memo-table* (node-id a) "result" result)
               ;; if there exits a node below this node on the stack, add this node to 
               ;; that node's successors and that node to this node's predecessors
               (when (not (empty? (cdr (unbox stack))))
                 (node-update *memo-table* 
                              (car (cdr (unbox stack))) 
                              "successors" 
                              (edge 'n
                                    (node-id a)
                                    result))
                 (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" 
                                         (car (cdr (unbox stack))) 
                                         (node-id a)))
                 (node-update *memo-table* (node-id a) 
                              "predecessors" (car (cdr (unbox stack)))))
               ;; remove this node from the top of the stack
               (set-box! stack (cdr (unbox stack)))
               (set-box! create-stack (cdr (unbox create-stack)))
               ;; return the result
               result)))]
    [(cell? a) 
     ;; update the stack by adding this cell to it
     (set-box! stack (cons (cell-id a) (unbox stack)))
     (set-box! create-stack (cons (cell-id a) (unbox create-stack)))
     ;; if there exits a node below this cell on the stack, add this cell to 
     ;; that node's successors and that node to this cell's predecessors
     (when (and (not (empty? (unbox stack)))
                (not (empty? (cdr (unbox stack)))))
       (node-update *memo-table* 
                    (car (cdr (unbox stack))) 
                    "successors" 
                    (edge 'c
                          (cell-id a) 
                          '()))
       (write-to-graph (format "[change]add edge~n[edge ~a ~a]~n" 
                               (car (cdr (unbox stack))) 
                               (cell-id a)))
       (let ([old-cell (hash-ref *cells* (cell-id a))])
         (hash-set! *cells* 
                    (cell-id a) 
                    (cell (cell-id a)
                          (cell-box old-cell)
                          (cons (car (cdr (unbox stack))) 
                                (cell-predecessors old-cell))
                          (cell-dirty old-cell)))))
     ;; extract the value of this cell
     (let ([result (unbox (cell-box a))])
       (set-box! stack (cdr (unbox stack)))
       (set-box! create-stack (cdr (unbox create-stack)))
       result)]
    [else a]))