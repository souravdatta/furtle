#lang typed/racket

(require typed/racket/draw
         typed/racket/gui)

(require "turtle-main.rkt")

(: default-line-width Positive-Integer)
(define default-line-width 1)

(: background-color String)
(define background-color "orange")

(: turtle-bitmap (->* (turtle Positive-Integer Positive-Integer)
                      (#:line-width Positive-Integer)
                      (Instance Bitmap%)))
(define (turtle-bitmap t width height #:line-width [line-width default-line-width])
  (let* ([target : (Instance Bitmap%) (make-bitmap width height)]
         [dc : (Instance Bitmap-DC%) (new bitmap-dc% [bitmap target])]
         [ops : (Listof Op) (reverse (turtle-ops t))])
    (send dc set-brush background-color 'solid)
    (send dc set-pen background-color line-width 'solid)
    (send dc draw-rectangle 0 0 width height)
    (send dc set-pen "black" line-width 'solid)
    (for ([x ops])
      (cond
        ((and (eq? (car (first x)) 'line)
              (eq? (cdr (first x)) 'pendown)) (send dc
                                                    draw-line
                                                    (car (second x)) (cdr (second x))
                                                    (car (third x)) (cdr (third x))))
        (else empty)))
    (send dc set-brush "red" 'solid)
    (send dc set-pen "red" 1 'solid)
    (send dc draw-ellipse
          (- (abs (turtle-tx t)) 4)
          (- (abs (turtle-ty t)) 4)
          8
          8)
    target))
    
(: draw (->* (TurtleF) (#:width Positive-Integer #:height Positive-Integer #:line-width Positive-Integer) (Instance Bitmap%)))
(define (draw tf #:width [width 800] #:height [height 800] #:line-width [line-width default-line-width])
  (let ([centerx (/ width 2)]
        [centery (/ height 2)])
    (turtle-bitmap (tf (make-turtle centerx centery))
                   width
                   height
                   #:line-width line-width)))

(: show! (->* (TurtleF) (#:width Positive-Integer #:height Positive-Integer #:line-width Positive-Integer) Void))
(define (show! tf #:width [width 800] #:height [height 800] #:line-width [line-width default-line-width])
  (let* ([centerx (/ width 2)]
         [centery (/ height 2)]
         [frame : (Instance Frame%) (new frame% [label "Furtle"] [width width] [height height])]
         [bitmap : (Instance Bitmap%) (turtle-bitmap (tf (make-turtle centerx centery))
                                                                                  width
                                                                                  height
                                                                                  #:line-width line-width)]
         [canvas : (Instance Canvas%) (new canvas%
                                           [parent frame]
                                           [min-width width]
                                           [min-height height]
                                           [paint-callback (lambda ([c : (Instance Canvas%)]
                                                                    [dc : (Instance DC<%>)])
                                                             (send dc clear)
                                                             (send dc set-brush background-color 'solid)
                                                             (send dc set-pen background-color line-width 'solid)
                                                             (send dc draw-rectangle 0 0 (send frame get-width) (send frame get-height))
                                                             (send dc draw-bitmap
                                                                   bitmap
                                                                   (- (/ (send frame get-width) 2) (/ width 2))
                                                                   (- (/ (send frame get-height) 2) (/ height 2))))])])
    (send frame show #t)))

  
(provide (all-defined-out))
