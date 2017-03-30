#lang typed/racket


(define-type Op (U (List (Pairof 'line (U 'penup 'pendown))  (Pairof Real Real) (Pairof Real Real))
                   (List (Pairof 'arc (U 'penup 'pendown)) (Pairof Real Real) (Pairof Real Real) Real)))
                          
(struct turtle ([tx : Real]
                [ty : Real]
                [angle : Real]
                [penstate : (U 'penup 'pendown)]
                [ops : (Listof Op)]))

(define-type TurtleF (-> turtle turtle))

(: fd (-> Real turtle turtle))
(define (fd len t)
  (let* ([tx (turtle-tx t)]
         [ty (turtle-ty t)]
         [rad-angle (degrees->radians (turtle-angle t))]
         [newx (- tx (* len (sin rad-angle)))]
         [newy (- ty (* len (cos rad-angle)))])
    (turtle newx newy
            (turtle-angle t)
            (turtle-penstate t)
            (cons (list (cons 'line (turtle-penstate t))
                        (cons tx ty)
                        (cons newx newy))
                  (turtle-ops t)))))

(: bk (-> Real turtle turtle))
(define (bk len t)
  (fd (- len) t))

(: rt (-> Real turtle turtle))
(define (rt angle t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (- (turtle-angle t) angle)
          (turtle-penstate t)
          (turtle-ops t)))

(: lt (-> Real turtle turtle))
(define (lt angle t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (+ (turtle-angle t) angle)
          (turtle-penstate t)
          (turtle-ops t)))

(: turtle-compose (-> (Listof TurtleF)
                      TurtleF))
(define (turtle-compose tfs)
  (cond
    ((empty? tfs) (lambda ([t : turtle]) t))
    ((= (length tfs) 1) (car tfs))
    (else (foldl #{compose :: (-> TurtleF
                                  TurtleF
                                  TurtleF)} (car tfs) (cdr tfs)))))

(: turtle-compose-n (-> Real (Listof TurtleF) TurtleF))
(define (turtle-compose-n n tfs)
  (let ([composed (turtle-compose tfs)])
    (turtle-compose (for/list : (Listof (-> turtle turtle)) ([x (in-range n)])
                      composed))))

(: arc-r (-> Real Real turtle turtle))
(define (arc-r angle radius t)
  (let ([fn (turtle-compose-n angle (list (curry fd (/ (* 2 pi radius) 360))
                                          (curry rt 1)))])
    (fn t)))

(: arc-l (-> Real Real turtle turtle))
(define (arc-l angle radius t)
  (let ([fn (turtle-compose-n angle (list (curry fd (/ (* 2 pi radius) 360))
                                          (curry lt 1)))])
    (fn t)))

(: pu (-> turtle turtle))
(define (pu t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          'penup
          (turtle-ops t)))

(: pd (-> turtle turtle))
(define (pd t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          'pendown
          (turtle-ops t)))
        
(define-syntax t<
  (syntax-rules ()
    [(_ fn) fn]
    [(_ fn a1) (curry fn a1)]
    [(_ fn a1 a2 ...) (t< (curry fn a1) a2 ...)]))

(: turtles (->* () () #:rest TurtleF TurtleF))
(define (turtles . tfs)
  (turtle-compose tfs))

(: repeat (->* (Real) () #:rest TurtleF TurtleF))
(define (repeat n . tfs)
  (turtle-compose-n n tfs))

(: make-turtle (-> Real Real turtle))
(define (make-turtle xpos ypos)
  (turtle xpos ypos 0 'pendown empty))


(provide
 (struct-out turtle)
 (except-out
  (all-defined-out)
  turtle-compose
  turtle-compose-n))


