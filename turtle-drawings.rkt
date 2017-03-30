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
        ((and (eq? (car (first x)) 'line)
              (eq? (cdr (first x)) 'pendown)) (send dc
                                                    draw-line
                                                    (car (second x)) (cdr (second x))
                                                    (car (third x)) (cdr (third x))))
        (else empty)))
    target))
    
(: draw (->* (TurtleF) (#:width Positive-Integer #:height Positive-Integer) (Instance Bitmap%)))
(define (draw tf #:width [width 800] #:height [height 800])
  (let ([centerx (/ width 2)]
        [centery (/ height 2)])
    (turtle-bitmap (tf (make-turtle centerx centery))
                   width
                   height)))

(provide (all-defined-out))
