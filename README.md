furtle
======

Is a WIP functional turtle graphics implementation.

Example

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
![all text](https://github.com/souravdatta/furtle/raw/master/fib-tree-example.png "Fibonacci tree")

Another example

    #lang typed/racket
    
    (require furtle)
    
    (: line (-> Integer Real TurtleF))
    (define (line count length)
        (if (= count 0)
            (turtles (t< fd length))
            (let ([count (- count 1)])
                 (turtles (line count length)
                 (t< lt 60)
                 (line count length)
                 (t< rt 120)
                 (line count length)
                 (t< lt 60)
                 (line count length)))))

    (: koch (-> Integer Real TurtleF))
    (define (koch count length)
      (turtles (t< rt 30)
               (line count length)
               (t< rt 120)
               (line count length)
               (t< rt 120)
               (line count length)))
  
    
    (draw (koch 5 5) #:height 4000 #:width 4000)
![alt text](https://github.com/souravdatta/furtle/raw/master/koch-example.png "Koch curve")

       

