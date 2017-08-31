#lang scribble/manual

@(require (for-label racket
					 "../main.rkt"))

@title{Furtle - a library for turtles}

@defmodule[furtle]

@centered{
	@italic{"It's TurtleF all the way down!"}
	@smaller{-- the killer cyborg from planet Rech-tangular}
}

furtle is a small Typed Racket library that lets you create turtle geometry in a mostly functional way.

@table-of-contents[]

@section{Overview}

The recomended way to create graphics with this library is to write expressions that result in a (-> turtle turtle), aka TurtleF, and then composing them with two primitive composers - turtles and repeat. However, all the primitive geometry functions along with functions that create and draws turtles are exposed, so if you need to customize the process of turtle drawing, you can directly use those as well. Lets dive into an example.

We want to create a drawing of a fibonacci tree of depth 14. Here's the way to proceed.

@racketblock[
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
]

The function fib-tree is called recursively to create the lower depth trees. There's a differentiation between how we call a turtle primitive such as fd and our own function fib-tree. This is because, the fd function can double down as a one that takes a turtle in its second argument and return the new turtle. But, in it's (t< fd n) form, it simply returns a TurtleF which on giving a concrete turtle will create a new turtle. The weird t< is simply a macro on top of curry library function.
A (t< fn-name arg1 arg2 ...) actually gets transformed into (λ ([t : turtle]) : turtle (fn-name arg1 arg2 ... t)). If you do not want to use this form, there are longer forms of the primitives which does the same operation but does not require the extra (t< ...) form. However, it is preferable to use the longer names [(forward 100) instead of (t< fd 100)] as they are clearer and also work in an untyped module. As an example, the above function can be re-written as:

@racketblock[
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
]


@margin-note{The thing to notice is that this is not interactive as in a typical Logo software. The drawing is created at the end of running the program - which is inconvenient but currently that is how it is.}

Now, this definition of fib-tree does not draw anything -  it merely creates a a specification of sorts, that when passed to draw or show! function, will draw the final scene.

Here's how we do that - 

@racketblock[
	(show! (fib-tree 10))
]

This will open a new window with the drawing and a red triangular turtle indicator. As fib-tree is another TurtleF, we can use it to further compose new drawing spec.

@racketblock[
(show! (turtles
        (fib-tree 10)
        (left 180)
        (fib-tree 10)))]

Or,

@racketblock[
(show! (repeat 4
               (fib-tree 10)
               (right 90)))]

Or,

@racketblock[
(show! (turtles
        (repeat 4
               (fib-tree 10)
               (right 90))
        (repeat 4
               (fib-tree 14)
               (right 90))))]


@section{Turtle move functions}

@defproc[(fd (n real?) (t turtle?))
			turtle?]{Forwards the turtle by n (Real). Aliased as foward.}

@defproc[(forward (n real?))
			TurtleF]{Forwards the turtle by n (Real).}

@defproc[(move (new-x real?) (new-y real?))
         TurtleF]{Moves the turtle to absolute coordinate (new-x, new-y), and if pen is down, draws a line.}

@defproc[(bk (n real?) (t turtle?))
			turtle?
]{Moves backwards by n (Real). Aliased as back.}

@defproc[(back (n real?))
			TurtleF
]{Moves backwards by n (Real).}

@defproc[
	(rt (ang real?) (t turtle?))
		turtle?
]{Rotates right by ang (Real). Aliased as right.}

@defproc[
	(right (ang real?))
		TurtleF
]{Rotates right by ang (Real).}

@defproc[
	(lt (ang real?) (t turtle?))
		turtle?
]{Rotates left by ang (Real). Aliased as left.}

@defproc[
	(left (ang real?))
		TurtleF
]{Rotates left by ang (Real).}

@defproc[
	(pu (t turtle?))
		turtle?
]{Pen up, no drawing but moves as is. Aliased as penup.}

@defproc[
	(penup)
		TurtleF
]{Pen up, no drawing but moves as is.}

@defproc[
	(pd (t turtle?))
		turtle?
]{Pen down, draws. Aliased as pendown.}

@defproc[
	(pendown)
		TurtleF
]{Pen down, draws.}

@defproc[
	(hide (t turtle?))
		turtle?
]{Hides the triangular turtle.}

@defproc[
	(turtle-hide)
		TurtleF
]{Hides the triangular turtle.}

@defproc[
	(show (t turtle?))
		turtle?
]{Shows the triangular turtle.}

@defproc[
	(turtle-show)
		TurtleF
]{Shows the triangular turtle.}

@defproc[
	(arc-l (angle real?) (radius real?) (t turtle?))
		turtle?
]{Moves turtle in an arc towards left with given angle and radius.}

@defproc[
	(arc-left (angle real?) (radius real?))
		TurtleF
]{Moves turtle in an arc towards left with given angle and radius.}

@defproc[
	(arc-r (angle real?) (radius real?) (t turtle?))
		turtle?
]{Moves turtle in an arc towards right with given angle and radius.}

@defproc[
	(arc-right (angle real?) (radius real?))
		TurtleF
]{Moves turtle in an arc towards right with given angle and radius.}

@defproc[
	(arc (angle real?) (radius real?) (t turtle?))
		turtle?
]{Draws an arc with turtle at center with given start angle and radius. The arc always starts on right side of turtle.}

@defproc[
	(sarc (angle real?) (radius real?))
		TurtleF
]{Draws an arc with turtle at center with given start angle and radius. The arc always starts on right side of turtle.}

@defproc[
         (pen-width (n positive-integer?))
         TurtleF
]{Sets the pen width of the turtle to the given postive integer n. Note that save/restore do not consider pen width.}

@defproc[
         (pen-color (n string?))
         TurtleF
]{Sets the color of the turtle to color identified by given string. If the string does not specify a color it will throw a runtime exception. Note that save/restore do not consider pen color.}

@defproc[
	(turtles (f1 TurtleF) ...)
		TurtleF
]{Takes any number of TurtleF forms f1 ... and composes into a single function of type TurtleF.}

@defproc[
	(repeat (n integer?) (f1 TurtleF) ...)
		TurtleF
]{Takes a Positive-Integer n and any number of TurtleF forms f1 ... and creates a TurtleF which is the compositing of f1 ... repeated n times.}

@defproc[
         (save)
         TurtleF
]{Saves the current state of the turtle - x, y, angle, pen state and arrow visibility on to a stack.}

@defproc[
         (restore)
         TurtleF
]{Restores the current state of the turtle - x, y, angle, pen state and arrow visibility from the top of the stack.}


@section{Various types in the library}

@defstruct[
	turtle ([tx real?]
			[ty real?]
			[angle real?]
			[penstate (or/c 'penup 'pendown)]
			[visible boolean?]
			[ops list?])

]{The main struct which holds the current state of the turtle. Every operation works on a turtle struct and creates a new one with the drawing operation cons'd to its list of ops.}

@defthing[TurtleF (turtle? . -> . turtle?)]{
	The basic type of every primitive operation as well as user defined function.	
}

@defproc[
         (turtle-from (t turtle?)
                      (#:tx tx (or/c #f real?) #f)
                      (#:ty ty (or/c #f real?) #f)
                      (#:angle angle (or/c #f real?) #f)
                      (#:penstate penstate (or/c #f 'penup 'pendown) #f)
                      (#:visible visible (or/c 'unused boolean?) 'unused))
         turtle?]{Returns a new turtle struct from a copy of t replacing one or more values passed as keyword arguments.}

@defproc[
	(t< (op symbol?) (a any/c) ...)
	TurtleF
]{A macro that converts a function that takes as the last argument a turtle object to a curried version with type TurtleF.}

@section{Turtle drawing functions}

@defproc[
	(draw 
		(tf TurtleF)
		(#:height height integer? 800)
		(#:width width integer? 800)
		(#:pen-width line-width integer? 1)
                (#:pen-color pen-width string? "black")
                (#:background-color background-color string? "orange")
	)
	(Instance Bitmap%)
]{Takes a TurtleF and returns a bitmap image of the drawing. Default width and height is 800x800.}

@defproc[
	(show! 
		(tf TurtleF)
		(#:height height integer? 800)
		(#:width width integer? 800)
		(#:pen-width line-width integer? 1)
                (#:pen-color pen-width string? "black")
                (#:background-color background-color string? "orange")
	)
	void?
]{Takes a TurtleF and opens a window with the drawing in it.}
