#lang typed/racket

(require typed/racket/draw
         typed/racket/gui)

(require/typed racket/draw/arrow
               [draw-arrow (->* ((Instance DC<%>) Real Real Real Real Real Real)
                                (#:pen-width Real
                                 #:arrow-head-size Real
                                 #:arrow-root-radius Real)
                                Void)])

(require "turtle-main.rkt")

(: default-line-width Positive-Integer)
(define default-line-width 1)

(: background-color String)
(define background-color "orange")

(: turtle-draw (-> turtle Positive-Integer Positive-Integer (Instance DC<%>) Positive-Integer Void))
(define (turtle-draw t width height dc line-width)
  (let ([ops : (Listof Op) (reverse (turtle-ops t))])
    (send dc set-brush background-color 'solid)
    (send dc set-pen "red" line-width 'solid)
    (send dc draw-rectangle 0 0 width height)
    (send dc set-pen "black" line-width 'solid)
    (send dc set-brush background-color 'transparent)
    (for ([x ops])
      (cond
        ((and (eq? (car (first x)) 'line)
              (eq? (cdr (first x)) 'pendown)) (send dc
                                                    draw-line
                                                    (car (second x)) (cdr (second x))
                                                    (car (third x)) (cdr (third x))))
        ((and (eq? (car (first x)) 'arc)
              (eq? (cdr (first x)) 'pendown)) (send dc
                                                    draw-arc
                                                    (second x) (third x)
                                                    (abs (fourth x)) (abs (fifth x))
                                                    (sixth x) (seventh x)))
        (else empty)))
    (when (turtle-visible t)
      (send dc set-brush "red" 'solid)
      (send dc set-pen "red" 1 'solid)
      (draw-arrow dc
                  (turtle-tx t)
                  (turtle-ty t)
                  (- (turtle-tx t) (* 4 (sin (degrees->radians (turtle-angle t)))))
                  (- (turtle-ty t) (* 4 (cos (degrees->radians (turtle-angle t)))))
                  1
                  1
                  #:arrow-root-radius 0
                  #:arrow-head-size 12))))

(: turtle-bitmap (->* (turtle Positive-Integer Positive-Integer)
                      (#:line-width Positive-Integer)
                      (Instance Bitmap%)))
(define (turtle-bitmap t width height #:line-width [line-width default-line-width])
  (let* ([target : (Instance Bitmap%) (make-bitmap width height)]
         [dc : (Instance Bitmap-DC%) (new bitmap-dc% [bitmap target])])
    (turtle-draw t width height dc line-width)     
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
  (let* ([frame : (Instance Frame%) (new frame% [label "Furtle"] [width width] [height height])]
         [canvas : (Instance Canvas%) (new canvas%
                                           [parent frame]
                                           [min-width width]
                                           [min-height height]
                                           [paint-callback (lambda ([c : (Instance Canvas%)]
                                                                    [dc : (Instance DC<%>)])
                                                             (let* ([fwidth : Positive-Integer (+ 1 (send frame get-width))]
                                                                    [fheight : Positive-Integer (+ 1 (send frame get-height))])
                                                               (send dc clear)
                                                               (turtle-draw (tf (make-turtle (/ fwidth 2)
                                                                                             (/ fheight 2)))
                                                                            fwidth
                                                                            fheight
                                                                            dc
                                                                            line-width)))])])
    (send frame show #t)))

  
(provide (all-defined-out))
