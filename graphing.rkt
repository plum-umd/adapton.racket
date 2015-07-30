#lang racket

;; Tools for graphing adapton

(require "data-structures.rkt")

(provide (all-defined-out))


;; ============= GRAPH ===============

(displayln (current-directory))

(define graphing-on #f)

(define file (string-append (~a (current-directory)) "graph.gmv"))
(when graphing-on
  (display-to-file (format "[styleselect colors]~n")
                   file
                   #:mode 'text
                   #:exists 'replace))

(define (write-to-graph string)
  (when graphing-on
    (display-to-file string	 
                     file
                     #:mode 'text
                     #:exists 'append)))

(when graphing-on (write-to-graph (format "[state]make red~n")))

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