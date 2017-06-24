#lang typed/racket


(require "../main.rkt")

;; example 1

(: spiral (-> Integer Integer TurtleF))
(define (spiral n l)
  (if (< n l)
      (turtles
       (t< fd n)
       (t< lt 70)
       (spiral (+ n 1) l))
      (turtles)))

(draw (turtles (t< hide) (spiral 1 800)))

;; example 2

(: lines (-> Real TurtleF))
(define (lines rn)
  (if (> rn 4.0)
      (turtles (forward rn)
               (penup)
               (back (/ rn 2))
               (pendown)
               (left 60)
               (lines (/ rn 2))
               (right 120)
               (lines (/ rn 2))
               (left 60)
               (penup)
               (back (/ rn 2))
               (pendown))
      (turtles)))


(draw (turtles (turtle-hide) (lines 200) (right 180) (lines 200)))

(draw (turtles
       save
       (forward 40)
       (sarc 360 10)
       (right 45)
       (forward 40)
       save ; save current state of turtle
       (λ ([t : turtle])
         ; now make a new turtle at a new position
         (turtle-from t
                      #:tx (+ 50 (turtle-tx t))
                      #:ty (+ 50 (turtle-ty t))))
       ; continue with the new turtle
       (forward 40)
       (sarc 360 10)
       (right 45)
       (forward 40)
       ; restore saved turtle
       restore
       (forward 80)
       (sarc 360 20)
       (right 45)
       (forward 80)
       restore
       (sarc 360 20)
       (forward 80)
       (sarc 360 20)
       (right 45)
       (forward 80)))

