#lang info
(define collection "furtle")
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "draw-lib"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/furtle.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(sourav))
