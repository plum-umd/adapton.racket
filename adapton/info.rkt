#lang info

;; Adapton without names, see 
;; http://www.cs.umd.edu/~hammer/adapton/adapton-pldi2014.pdf
(define version "1.0")
(define name "Adapton")

(define collection 'multi)
(define deps '("base" "rackunit-lib"))
(define build-deps (list "scribble-lib" "racket-doc"))

(define primary-file "main.rkt")

(define blurb '("Adapton - Composable, Demand-Driven Incremental Computation"))

(define pkg-authors '(cmentzer))

(define scribblings '(("adapton.scrbl")))
