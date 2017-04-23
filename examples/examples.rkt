#lang typed/racket


(require furtle)

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
      (turtles (t< fd rn)
               (t< pu)
               (t< bk (/ rn 2))
               (t< pd)
               (t< lt 60)
               (lines (/ rn 2))
               (t< rt 120)
               (lines (/ rn 2))
               (t< lt 60)
               (t< pu)
               (t< bk (/ rn 2))
               (t< pd))
      (turtles)))


(draw (turtles (t< hide) (lines 200) (t< rt 180) (lines 200)))

