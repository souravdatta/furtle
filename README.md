furtle
======

Is a WIP functional turtle graphics implementation.

##Example

    #lang typed/racket
    
    (require furtle)
    
    (: fib-tree (-> Real TurtleF))
    (define (fib-tree depth)
    (if (>= depth 1)
          (turtles (t< fd 15)
                   (t< lt 15)
                   (fib-tree (- depth 1))
                   (t< rt 30)
                   (fib-tree (- depth 2))
                   (t< lt 15)
                   (t< bk 15))
          (turtles)))
    
    (draw (fib-tree 14))
   
  
