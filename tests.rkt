#lang racket
;; This file contains a number of tests for adapton
;; Most of this file will be commented out, to prevent unnecessary additions to the 
;; global hash-tables.

(require "merge-sort.rkt"
         "adapton.rkt"
         "tools-for-testing.rkt"
         "data-structures.rkt"
         "memo-table-modification-tools.rkt")

;; A cell containing a 1 element list
#;(define test-cell (make-cell (cons (cons (make-cell 1) empty)
                                   empty)))

;; A cell containing a number
;(define test-cell-2 (make-cell 1))

;; The list '((3) (2) (1))
;(define tiny-input (m-cons 3 (m-cons 2 (m-cons 1 empty))))

;(define small-input (m-cons 5 (m-cons 4 (m-cons 3 (m-cons 2 (m-cons 1 empty))))))

;; The list '((3) (6) (9) (2) (4) (5) (8) (1))
#;(define less-small-input (make-cell 
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


;; The list of singleton lists from 100 - 1
;(define medium-input (build-input 100))
;(define longer-input (build-input 1000))
;(define longer-list (build-list 1000))
;(define longer-list-mutated (append (remove '(1) longer-list) (list (list 0))))
;(define longest-input (build-trivial-input 10000))
;(define longest-list (build-list 10000))
;(define longest-list-mutated (append (remove '(1) longest-list) (list (list 0))))

;(define a (merge-sort tiny-input))
;(define b (merge-sort small-input))
;(define b2 (merge-sort less-small-input))
;(define c (merge-sort medium-input))
;(define d (merge-sort longer-input))
;(define e (merge-sort longest-input))

#;(define mergesort-tests
  (test-suite
   "testing correctness of mergesort for adapton"
   
   (check-equal? (unbox (cell-box (hash-ref *cells* 3))) 1 "test creation of *cells* entry")
   (check-equal? (read-cell test-cell-2) 1 "test read-cell function")
   (check-exn    exn:fail? (λ () (read-cell/update test-cell-2)) "read-cell/update with no stack throws exn")
   (check-equal? (force (car (car (force test-cell)))) 1 "test extracting value from cell")
   (check-equal? (force (car (car (force (cdr (force tiny-input)))))) 2 "test force on cell in list")
   (check-equal? (print-list-from-delayed-list tiny-input) '((3) (2) (1)) "test list structure")
   (check-equal? (get-list-from-mergesort (force a)) 
                 '(1 2 3)
                 "test mergesort on short list")
   (check-equal? (get-list-from-mergesort (force b))
                 '(1 2 3 4 5 6 8 9)
                 "test mergesort on long list")
   (check-not-exn (λ () (set-cell! 15 0)) "test set-cell!")
   (check-equal? (unbox (cell-box (hash-ref *cells* 15))) 0 "test effect of set-cell!")
   (check-equal? (andmap node-dirty 
                         (map (λ (a) (hash-ref *memo-table* a)) 
                              (cell-predecessors (hash-ref *cells* 15))))
                 #t 
                 "check proper node dirtying after set-cell!")
   (check-equal? (get-list-from-mergesort (force (hash-ref *memo-table* (node-id b))))
                 '(0 1 2 3 4 6 8 9)
                 "re-build list after set-cell!")))

#;(define small-timed-tests
  (test-suite
   "testing performance of adapton mergesort vs regular mergesort"
   
   (check-equal? (time (get-list-from-mergesort (force b)))
                 (time (merge-sort-default '((3) (6) (9) (2) (4) (5) (8) (1))))
                 "compare first sort times")
   (check-equal? (time (get-list-from-mergesort (force b)))
                 (time (merge-sort-default '((3) (6) (9) (2) (4) (5) (8) (1))))
                 "compare second sort times")
   (check-not-exn (λ () (set-cell! 15 0))
                  "mutate input")
   (check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id b)))))
                 (time (merge-sort-default '((3) (6) (9) (2) (4) (0) (8) (1))))
                 "compare times after mutation")))

#;(define medium-timed-tests
  (test-suite
   "testing performance of adapton mergesort vs regular mergesort"
   
   (check-equal? (time (get-list-from-mergesort (force c)))
                 (time (merge-sort-default (build-list 100)))
                 "compare first sort times")
   (check-equal? (time (get-list-from-mergesort (force c)))
                 (time (merge-sort-default (build-list 100)))
                 "compare second sort times")
   #;(check-not-exn (λ () (set-cell! 224 0))
                    "mutate input")
   (check-not-exn (λ () (set-cell! 26 0))
                  "mutuate input")
   #;(check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id c)))))
                   (time (merge-sort-default (cons (cons 0 empty) (build-list 99))))
                   "compare times after mutation")
   (check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id c)))))
                 (time (merge-sort-default (append (remove '(1) (build-list 100)) (list (list 0)))))
                 "compare times after mutation")))

#;(define long-timed-tests
  (test-suite
   "testing performance of adapton mergesort vs regular mergesort"
   
   (check-equal? (time (get-list-from-mergesort (force d)))
                 (time (merge-sort-default longer-list))
                 "compare first sort times")
   (check-equal? (time (get-list-from-mergesort (force d)))
                 (time (merge-sort-default longer-list))
                 "compare second sort times")
   #;(check-not-exn (λ () (set-cell! 224 0))
                    "mutate input")
   (check-not-exn (λ () (set-cell! 236 0))
                  "mutuate input")
   #;(check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id d)))))
                   (time (merge-sort-default (cons (cons 0 empty) longest-list)))
                   "compare times after mutation")
   (check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id d)))))
                 (time (merge-sort-default longer-list-mutated))
                 "compare times after mutation")))

#;(define longest-timed-tests
    (test-suite
     "testing performance of adapton mergesort vs regular mergesort"
     
     (check-equal? (time (get-list-from-mergesort (force e)))
                   (time (merge-sort-default longest-list))
                   "compare first sort times")
     (check-equal? (time (get-list-from-mergesort (force e)))
                   (time (merge-sort-default longest-list))
                   "compare second sort times")
     (check-not-exn (λ () (set-cell! 36 0))
                      "mutuate input")
     (check-equal? (time (get-list-from-mergesort (force (hash-ref *memo-table* (node-id e)))))
                     (time (merge-sort-default longest-list-mutated))
                     "compare times after mutation")))


;(run-tests long-timed-tests)

