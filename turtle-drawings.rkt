#lang typed/racket

(require typed/racket/draw)

(require "turtle-main.rkt")

(: turtle-bitmap (->* (turtle Positive-Integer Positive-Integer)
                      ()
                      (Instance Bitmap%)))
(define (turtle-bitmap t width height)
  (let* ([target : (Instance Bitmap%) (make-bitmap width height)]
         [dc : (Instance Bitmap-DC%) (new bitmap-dc% [bitmap target])]
         [ops : (Listof Op) (reverse (turtle-ops t))])
    (for ([x ops])
      (cond
        ((eq? (first x) 'line) (send dc
                                     draw-line
                                     (car (second x)) (cdr (second x))
                                     (car (third x)) (cdr (third x))))
        (else empty)))
    target))
    
(: draw (-> TurtleF (Instance Bitmap%)))
(define (draw tf)
  (let* ([width 800]
         [height 800]
         [centerx 400]
         [centery 400])
    (turtle-bitmap (tf (make-turtle centerx centery))
                   width
                   height)))

(provide (all-defined-out))
