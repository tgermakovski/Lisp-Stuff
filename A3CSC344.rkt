#lang racket


;task 1a

( define (lambdaaa n)
( define x ( + n 1 ) )
( define y ( + n 2 ) )
( define output ( list n x y ) )
output
)

;task 1b

;in body or in console? unnamed function? vsmyslje?


( define ( lambda2 a b c )
( define output ( list c b a ) )
output
)






;task 1c

( define (lambda3 a b)
   (define output (random a b))
   output
   )
  

;task 3a

( define ( sampler )
( display "(?): " )
( define the-list ( read ) )
( define the-element
( list-ref the-list ( random ( length the-list ) ) )
)
( display the-element ) ( display "\n" )
( sampler )
)


;task 3b

( define ( color_thing )
( display "(?): " )
( define the-list ( read ) )
( define the-element
( list-ref the-list ( random ( length the-list ) ) )
)
( display the-element ) ( display "\n" )
( color_thing )
)