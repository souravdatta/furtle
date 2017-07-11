#lang typed/racket


(define-type Op (U (List (Pairof 'line (U 'penup 'pendown))  (Pairof Real Real) (Pairof Real Real))
                   (List (Pairof 'arc (U 'penup 'pendown)) Real Real Real Real Real Real)
                   (List (Pairof 'pen-color String))
                   (List (Pairof 'pen-width Positive-Integer))))
                  
(struct turtle ([tx : Real]
                [ty : Real]
                [angle : Real]
                [penstate : (U 'penup 'pendown)]
                [visible : Boolean]
                [saves : (Listof (List Real Real Real (U 'penup 'pendown) Boolean))]
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
            (turtle-visible t)
            (turtle-saves t)
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
          (turtle-visible t)
          (turtle-saves t)
          (turtle-ops t)))

(: lt (-> Real turtle turtle))
(define (lt angle t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (+ (turtle-angle t) angle)
          (turtle-penstate t)
          (turtle-visible t)
          (turtle-saves t)
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

(: arc (-> Real Real turtle turtle))
(define (arc angle rad t)
  (let ([x (- (turtle-tx t) rad)]
        [y (- (turtle-ty t) rad)]
        [width (* rad 2)]
        [height (* rad 2)]
        [start-angle (degrees->radians (+ (turtle-angle t) 0))]
        [end-angle (degrees->radians (+ (turtle-angle t) angle))])
    (turtle (turtle-tx t)
            (turtle-ty t)
            (turtle-angle t)
            (turtle-penstate t)
            (turtle-visible t)
            (turtle-saves t)
            (cons (list (cons 'arc (turtle-penstate t))
                        x y
                        width height
                        start-angle end-angle)
                  (turtle-ops t)))))
                        

(: pu TurtleF)
(define (pu t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          'penup
          (turtle-visible t)
          (turtle-saves t)
          (turtle-ops t)))

(: pd TurtleF)
(define (pd t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          'pendown
          (turtle-visible t)
          (turtle-saves t)
          (turtle-ops t)))

(: hide TurtleF)
(define (hide t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          (turtle-penstate t)
          #f
          (turtle-saves t)
          (turtle-ops t)))

(: show TurtleF)
(define (show t)
  (turtle (turtle-tx t)
          (turtle-ty t)
          (turtle-angle t)
          (turtle-penstate t)
          #t
          (turtle-saves t)
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
  (turtle xpos ypos 0 'pendown #t empty empty))

(: turtle-from (->* (turtle)
                    (#:tx (U False Real)
                     #:ty (U False Real)
                     #:angle (U False Real)
                     #:penstate (U False 'penup 'pendown)
                     #:visible (U 'unused Boolean))
                    turtle))
(define (turtle-from t
                     #:tx [tx #f]
                     #:ty [ty #f]
                     #:angle [angle #f]
                     #:penstate [penstate #f]
                     #:visible [visible 'unused])
  (turtle (if tx tx (turtle-tx t))
          (if ty ty (turtle-ty t))
          (if angle angle (turtle-angle t))
          (if penstate penstate (turtle-penstate t))
          (if (eqv? visible 'unused) (turtle-visible t) (if visible visible #f))
          (turtle-saves t)
          (turtle-ops t)))
                     
(: forward (-> Real TurtleF))
(define (forward len) (t< fd len))

(: back (-> Real TurtleF))
(define (back len) (t< bk len))

(: right (-> Real TurtleF))
(define (right len) (t< rt len))

(: left (-> Real TurtleF))
(define (left len) (t< lt len))

(: penup (-> TurtleF))
(define (penup) (t< pu))

(: pendown (-> TurtleF))
(define (pendown) (t< pd))

(: turtle-hide (-> TurtleF))
(define (turtle-hide) (t< hide))

(: turtle-show (-> TurtleF))
(define (turtle-show) (t< show))

(: arc-right (-> Real Real TurtleF))
(define (arc-right x y)
  (t< arc-r x y))

(: arc-left (-> Real Real TurtleF))
(define (arc-left x y)
  (t< arc-l x y))

(: sarc (-> Real Real TurtleF))
(define (sarc x y)
  (t< arc x y))

(: pen-width (-> Positive-Integer TurtleF))
(define (pen-width n)
  (λ ([t : turtle])
    (turtle (turtle-tx t)
            (turtle-ty t)
            (turtle-angle t)
            (turtle-penstate t)
            (turtle-visible t)
            (turtle-saves t)
            (cons (list (cons 'pen-width n)) (turtle-ops t)))))

(: pen-color (-> String TurtleF))
(define (pen-color s)
  (λ ([t : turtle])
    (turtle (turtle-tx t)
            (turtle-ty t)
            (turtle-angle t)
            (turtle-penstate t)
            (turtle-visible t)
            (turtle-saves t)
            (cons (list (cons 'pen-color s)) (turtle-ops t)))))

(: save (-> TurtleF))
(define (save)
  (λ ([t : turtle]) : turtle
    (turtle
     (turtle-tx t)
     (turtle-ty t)
     (turtle-angle t)
     (turtle-penstate t)
     (turtle-visible t)
     (cons (list (turtle-tx t)
                 (turtle-ty t)
                 (turtle-angle t)
                 (turtle-penstate t)
                 (turtle-visible t))
           (turtle-saves t))
     (turtle-ops t))))

(: restore (-> TurtleF))
(define (restore)
  (λ ([t : turtle]) : turtle
    (if (not (empty? (turtle-saves t)))
        (let ([save (car (turtle-saves t))])
          (turtle
           (first save)
           (second save)
           (third save)
           (fourth save)
           (fifth save)
           (cdr (turtle-saves t))
           (turtle-ops t)))
        t)))

(define-syntax-rule (turtle-when cond e1 ...)
  (if cond
      (turtles e1 ...)
      (turtles)))

(provide
 (struct-out turtle)
 (except-out
  (all-defined-out)
  turtle-compose
  turtle-compose-n))


