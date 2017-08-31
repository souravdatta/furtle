# Furtle - a library for turtles

```racket
 (require furtle) package: furtle
```

_"It’s TurtleF all the way down!"_ – the killer cyborg from planet
Rech-tangular

furtle is a small Typed Racket library that lets you create turtle
geometry in a mostly functional way.

    1 Overview                    
                                  
    2 Turtle move functions       
                                  
    3 Various types in the library
                                  
    4 Turtle drawing functions    

## 1. Overview

The recomended way to create graphics with this library is to write
expressions that result in a \(-> turtle turtle\), aka TurtleF, and then
composing them with two primitive composers - turtles and repeat.
However, all the primitive geometry functions along with functions that
create and draws turtles are exposed, so if you need to customize the
process of turtle drawing, you can directly use those as well. Lets dive
into an example.

We want to create a drawing of a fibonacci tree of depth 14. Here’s the
way to proceed.

```racket
(require furtle)                         
                                         
(: fib-tree (-> Real TurtleF))           
(define (fib-tree depth)                 
    (if (>= depth 1)                     
          (turtles (t< fd 15)            
                   (t< lt 15)            
                   (fib-tree (- depth 1))
                   (t< rt 30)            
                   (fib-tree (- depth 1))
                   (t< lt 15)            
                   (t< bk 15))           
          (turtles)))                    
```

The function fib-tree is called recursively to create the lower depth
trees. There’s a differentiation between how we call a turtle primitive
such as fd and our own function fib-tree. This is because, the fd
function can double down as a one that takes a turtle in its second
argument and return the new turtle. But, in it’s \(t< fd n\) form, it
simply returns a TurtleF which on giving a concrete turtle will create a
new turtle. The weird t< is simply a macro on top of curry library
function. A \(t< fn-name arg1 arg2 ...\) actually gets transformed into
\(λ \(\[t : turtle\]\) : turtle \(fn-name arg1 arg2 ... t\)\). If you do
not want to use this form, there are longer forms of the primitives
which does the same operation but does not require the extra \(t< ...\)
form. However, it is preferable to use the longer names \[\(forward
100\) instead of \(t< fd 100\)\] as they are clearer and also work in an
untyped module. As an example, the above function can be re-written as:

```racket
(require furtle)                         
                                         
(: fib-tree (-> Real TurtleF))           
(define (fib-tree depth)                 
    (if (>= depth 1)                     
          (turtles (forward 15)          
                   (left 15)             
                   (fib-tree (- depth 1))
                   (right 30)            
                   (fib-tree (- depth 1))
                   (left 15)             
                   (back 15))            
          (turtles)))                    
```

> The thing to notice is that this is not interactive as in a typical Logo
> software. The drawing is created at the end of running the program -
> which is inconvenient but currently that is how it is.

Now, this definition of fib-tree does not draw anything -  it merely
creates a a specification of sorts, that when passed to draw or show!
function, will draw the final scene.

Here’s how we do that -

`(show!` `(fib-tree` `10))`

This will open a new window with the drawing and a red triangular turtle
indicator. As fib-tree is another TurtleF, we can use it to further
compose new drawing spec.

```racket
(show! (turtles        
        (fib-tree 10)  
        (left 180)     
        (fib-tree 10)))
```

Or,

```racket
(show! (repeat 4            
               (fib-tree 10)
               (right 90))) 
```

Or,

```racket
(show! (turtles             
        (repeat 4           
               (fib-tree 10)
               (right 90))  
        (repeat 4           
               (fib-tree 14)
               (right 90))))
```

## 2. Turtle move functions

```racket
(fd n t) -> turtle?
  n : real?        
  t : turtle?      
```

Forwards the turtle by n \(Real\). Aliased as foward.

```racket
(forward n) -> TurtleF
  n : real?           
```

Forwards the turtle by n \(Real\).

```racket
(move new-x new-y) -> TurtleF
  new-x : real?              
  new-y : real?              
```

Moves the turtle to absolute coordinate \(new-x, new-y\), and if pen is
down, draws a line.

```racket
(bk n t) -> turtle?
  n : real?        
  t : turtle?      
```

Moves backwards by n \(Real\). Aliased as back.

```racket
(back n) -> TurtleF
  n : real?        
```

Moves backwards by n \(Real\).

```racket
(rt ang t) -> turtle?
  ang : real?        
  t : turtle?        
```

Rotates right by ang \(Real\). Aliased as right.

```racket
(right ang) -> TurtleF
  ang : real?         
```

Rotates right by ang \(Real\).

```racket
(lt ang t) -> turtle?
  ang : real?        
  t : turtle?        
```

Rotates left by ang \(Real\). Aliased as left.

```racket
(left ang) -> TurtleF
  ang : real?        
```

Rotates left by ang \(Real\).

```racket
(pu t) -> turtle?
  t : turtle?    
```

Pen up, no drawing but moves as is. Aliased as penup.

```racket
(penup) -> TurtleF
```

Pen up, no drawing but moves as is.

```racket
(pd t) -> turtle?
  t : turtle?    
```

Pen down, draws. Aliased as pendown.

```racket
(pendown) -> TurtleF
```

Pen down, draws.

```racket
(hide t) -> turtle?
  t : turtle?      
```

Hides the triangular turtle.

```racket
(turtle-hide) -> TurtleF
```

Hides the triangular turtle.

```racket
(show t) -> turtle?
  t : turtle?      
```

Shows the triangular turtle.

```racket
(turtle-show) -> TurtleF
```

Shows the triangular turtle.

```racket
(arc-l angle radius t) -> turtle?
  angle : real?                  
  radius : real?                 
  t : turtle?                    
```

Moves turtle in an arc towards left with given angle and radius.

```racket
(arc-left angle radius) -> TurtleF
  angle : real?                   
  radius : real?                  
```

Moves turtle in an arc towards left with given angle and radius.

```racket
(arc-r angle radius t) -> turtle?
  angle : real?                  
  radius : real?                 
  t : turtle?                    
```

Moves turtle in an arc towards right with given angle and radius.

```racket
(arc-right angle radius) -> TurtleF
  angle : real?                    
  radius : real?                   
```

Moves turtle in an arc towards right with given angle and radius.

```racket
(arc angle radius t) -> turtle?
  angle : real?                
  radius : real?               
  t : turtle?                  
```

Draws an arc with turtle at center with given start angle and radius.
The arc always starts on right side of turtle.

```racket
(sarc angle radius) -> TurtleF
  angle : real?               
  radius : real?              
```

Draws an arc with turtle at center with given start angle and radius.
The arc always starts on right side of turtle.

```racket
(pen-width n) -> TurtleF
  n : positive-integer? 
```

Sets the pen width of the turtle to the given postive integer n. Note
that save/restore do not consider pen width.

```racket
(pen-color n) -> TurtleF
  n : string?           
```

Sets the color of the turtle to color identified by given string. If the
string does not specify a color it will throw a runtime exception. Note
that save/restore do not consider pen color.

```racket
(turtles f1 ...) -> TurtleF
  f1 : TurtleF             
```

Takes any number of TurtleF forms f1 ... and composes into a single
function of type TurtleF.

```racket
(repeat n f1 ...) -> TurtleF
  n : integer?              
  f1 : TurtleF              
```

Takes a Positive-Integer n and any number of TurtleF forms f1 ... and
creates a TurtleF which is the compositing of f1 ... repeated n times.

```racket
(save) -> TurtleF
```

Saves the current state of the turtle - x, y, angle, pen state and arrow
visibility on to a stack.

```racket
(restore) -> TurtleF
```

Restores the current state of the turtle - x, y, angle, pen state and
arrow visibility from the top of the stack.

## 3. Various types in the library

```racket
(struct turtle (tx ty angle penstate visible ops)
    #:extra-constructor-name make-turtle)        
  tx : real?                                     
  ty : real?                                     
  angle : real?                                  
  penstate : (or/c 'penup 'pendown)              
  visible : boolean?                             
  ops : list?                                    
```

The main struct which holds the current state of the turtle. Every
operation works on a turtle struct and creates a new one with the
drawing operation cons’d to its list of ops.

```racket
TurtleF : (turtle? . -> . turtle?)
```

The basic type of every primitive operation as well as user defined
function.

```racket
(turtle-from  t                              
             [#:tx tx                        
              #:ty ty                        
              #:angle angle                  
              #:penstate penstate            
              #:visible visible]) -> turtle? 
  t : turtle?                                
  tx : (or/c #f real?) = #f                  
  ty : (or/c #f real?) = #f                  
  angle : (or/c #f real?) = #f               
  penstate : (or/c #f 'penup 'pendown) = #f  
  visible : (or/c 'unused boolean?) = 'unused
```

Returns a new turtle struct from a copy of t replacing one or more
values passed as keyword arguments.

```racket
(t< op a ...) -> TurtleF
  op : symbol?          
  a : any/c             
```

A macro that converts a function that takes as the last argument a
turtle object to a curried version with type TurtleF.

## 4. Turtle drawing functions

```racket
(draw  tf                                                         
      [#:height height                                            
       #:width width                                              
       #:pen-width line-width                                     
       #:pen-color pen-width                                      
       #:background-color background-color]) -> (Instance Bitmap%)
  tf : TurtleF                                                    
  height : integer? = 800                                         
  width : integer? = 800                                          
  line-width : integer? = 1                                       
  pen-width : string? = "black"                                   
  background-color : string? = "orange"                           
```

Takes a TurtleF and returns a bitmap image of the drawing. Default width
and height is 800x800.

```racket
(show!  tf                                            
       [#:height height                               
        #:width width                                 
        #:pen-width line-width                        
        #:pen-color pen-width                         
        #:background-color background-color]) -> void?
  tf : TurtleF                                        
  height : integer? = 800                             
  width : integer? = 800                              
  line-width : integer? = 1                           
  pen-width : string? = "black"                       
  background-color : string? = "orange"               
```

Takes a TurtleF and opens a window with the drawing in it.
