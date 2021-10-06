#lang info
(define version "1.0")
(define scribblings '(("simple-matrix-manual.scrbl" ())))
(define collection "simple-matrix")

(define pkg-desc "Basic arithmetic with matrices.")

(define deps '("base" "sandbox-lib" "scribble-lib"))
(define build-deps '("rackunit-lib" "racket-doc"))
