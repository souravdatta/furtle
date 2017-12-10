#lang racket

(require furtle)

(define-syntax-rule (turtle-when cond e1 ...)
  (if cond
      (turtles e1 ...)
      (turtles)))


(provide turtle-when)
