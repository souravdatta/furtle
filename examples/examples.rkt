#lang typed/racket


(require furtle)
(require furtle/when)


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
  (turtle-when (> rn 4.0)
               (forward rn)
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
               (pendown)))


(draw (turtles (turtle-hide) (lines 200) (right 180) (lines 200)))

(draw (turtles
       (save)

       (pen-width 4)
       (pen-color "red")
       (forward 40)
       (sarc 360 10)
       (right 45)
       (forward 40)
       (save) ; save current state of turtle

       (pen-color "blue")
       (pen-width 2)
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
       (restore)

       (pen-color "black")
       (pen-width 1)
       (forward 80)
       (sarc 360 20)
       (right 45)
       (forward 80)
       (restore)
       (sarc 360 20)
       (sarc 360 10)
       (forward 80)
       (sarc 360 20)
       (sarc 360 10)
       (right 45)
       (forward 80)))

(draw (turtles
       (forward 100)
       (left 50)
       (forward 50)
       (right 45)
       (forward 25))
      #:background-color "orange"
      #:pen-width 2
      #:pen-color "red")

(draw (turtles
       (forward 100)
       (move 100 100)
       (penup)
       (move 200 200)
       (pendown)
       (move 300 300)))

(: fib-tree (-> Integer TurtleF))
(define (fib-tree depth)
  (turtle-when (>= depth 2)
               (save)
               (pen-width depth)
               (forward 40)
               (right 15)
               (fib-tree (- depth 2))
               (left 30)
               (fib-tree (- depth 1))
               (restore)))

(draw (turtles (turtle-hide) (pen-color "green") (fib-tree 14)))

