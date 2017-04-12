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
        (t< lt 180)
        (fib-tree 10)))]

Or,

@racketblock[
(show! (repeat 4
               (fib-tree 10)
               (t< rt 90)))]

Or,

@racketblock[
(show! (turtles
        (repeat 4
               (fib-tree 10)
               (t< rt 90))
        (repeat 4
               (fib-tree 14)
               (t< rt 90))))]


@section{Turtle move functions}

@defproc[(fd (n real?) (t turtle?))
			turtle?]

Forwards the turtle by n (Real). Aliased as foward.

@defproc[(bk (n real?) (t turtle?))
			turtle?
]

Moves backwards by n (Real). Aliased as back.

@defproc[
	(rt (ang real?) (t turtle?))
		turtle?
]

Rotates right by ang (Real). Aliased as right.

@defproc[
	(lt (ang real?) (t turtle?))
		turtle?
]

Rotates left by ang (Real). Aliased as left.

@defproc[
	(pu (t turtle?))
		turtle?
]

Pen up, no drawing but moves as is. Aliased as penup.

@defproc[
	(pd (t turtle?))
		turtle?
]

Pen down, draws. Aliased as pendown.

@defproc[
	(hide (t turtle?))
		turtle?
]

Hides the triangular turtle.

@defproc[
	(show (t turtle?))
		turtle?
]

Shows the triangular turtle.

@defproc[
	(arc-l (angle real?) (radius real?) (t turtle?))
		turtle?
]

Moves turtle in an arc towards left with given angle and radius.

@defproc[
	(arc-r (angle real?) (radius real?) (t turtle?))
		turtle?
]

Moves turtle in an arc towards right with given angle and radius.

@defproc[
	(arc (angle real?) (radius real?) (t turtle?))
		turtle?
]

Draws an arc with turtle at center with given start angle and radius. The arc always starts on right side of turtle.

@defproc[
	(turtles (f1 TurtleF) ...)
		TurtleF
]

Takes any number of TurtleF forms f1 ... and composes into a single function of type TurtleF.

@defproc[
	(repeat (n integer?) (f1 TurtleF) ...)
		TurtleF
]

Takes a Positive-Integer n and any number of TurtleF forms f1 ... and creates a TurtleF which is the compositing of f1 ... repeated n times.

@section{Various types in the library}

@defstruct[
	turtle ([tx real?]
			[ty real?]
			[angle real?]
			[penstate (or/c 'penup 'pendown)]
			[visible boolean?]
			[ops list?])

]

The main struct which holds the current state of the turtle. Every operation works on a turtle struct and creates a new one with the drawing operation cons'd to its list of ops.

@defthing[TurtleF (turtle? . -> . turtle?)]{
	The basic type of every primitive operation as well as user defined function.	
}


@defproc[
	(t< (op symbol?) (a any/c) ...)
	TurtleF
]

A macro that converts a function that takes as the last argument a turtle object to a curried version with type TurtleF.

@section{Turtle drawing functions}

@defproc[
	(draw 
		(tf TurtleF)
		(height integer? 800)
		(width integer? 800)
		(line-width integer? 1)
	)
	void?
]

Takes a TurtleF and returns a bitmap image of the drawing. Default width and height is 800x800.

@defproc[
	(show! 
		(tf TurtleF)
		(#height height integer? 800)
		(#width width integer? 800)
		(#line-width line-width integer? 1)
	)
	void?
]

Takes a TurtleF and opens a window with the drawing in it.
