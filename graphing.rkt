#lang racket

;; Tools for graphing adapton

(require "data-structures.rkt")

(provide (all-defined-out))


;; There is code throughout the repo that builds a visual representation 
;; of the DCG of your program. Obviously this code will effect performance, 
;; and so can be turned off by setting the parameter "graphing-on" to false 
;; (#f) in this file. If you choose to leave graphing enabled, 
;; the results of the graphing are automatically stored in the file graph.gmv 
;; in the same directory as this file. You can modify the location of the 
;; graph flie below.

;; see the readme for more information about graphing.

;; ============= GRAPH ===============

;; if graphing-on is set to #t, graphing is enabled.
;; it can be set to #f to disable all graph-related functions
(define graphing-on #f)

(when graphing-on
  (displayln "graphing is ON and this file is running in:")
  (displayln (current-directory)))

(when (not graphing-on)
  (displayln "graphing is OFF"))

;; ===================================================================

;; "file" contains the location of the file your graph is stored in,
;; default is the same location as this file. 

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

;; ===================================================================

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