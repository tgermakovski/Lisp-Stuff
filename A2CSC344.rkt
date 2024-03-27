#lang racket

(require 2htdp/image)

;; TASK 1
;; TASK 1
;; TASK 1

(define (random-color) ( color (rgb-value) (rgb-value) (rgb-value) ) )
(define (rc) (random-color))
(define (rgb-value) ( random 256 ) )

(define (house w h a b c)
  ( define hh ( / h 3 ) )
  ( define roof ( triangle w "solid" "gray" ) )  
  ( define floor1 ( rectangle w hh "solid" a) )
  ( define floor2 ( rectangle w hh "solid" b) )
  ( define floor3 ( rectangle w hh "solid" c) )
  ( define stack1 ( above roof floor3 floor2 floor1 ) )
  ( define stack2 ( above roof floor3 floor2 floor1 ) )
  stack1
)

(define (tract w h)
  ( define a (random-color))
  ( define b (random-color))
  ( define c (random-color))
  ( define hh ( / h 3 ) )
  ( define blank-b (rectangle 10 hh "solid" "white"))
  ( define blank-r (triangle 10 "solid" "white"))
  ( define roof ( triangle w "solid" "gray" ) )  
  ( define roof-row ( beside roof blank-r roof blank-r roof blank-r roof blank-r roof blank-r roof ) )  
  ( define block-a ( rectangle w hh "solid" a) )
  ( define block-b ( rectangle w hh "solid" b) )
  ( define block-c ( rectangle w hh "solid" c) )
  ( define floor1 ( beside block-c blank-b block-b blank-b block-c blank-b block-a blank-b block-b blank-b block-a))
  ( define floor2 ( beside block-b blank-b block-c blank-b block-a blank-b block-c blank-b block-a blank-b block-b))
  ( define floor3 ( beside block-a blank-b block-a blank-b block-b blank-b block-b blank-b block-c blank-b block-c))
  ( define stack1 ( above roof-row floor3 floor2 floor1 ) )
  ( define stack2 ( above roof-row floor3 floor2 floor1 ) )
  stack1
)

;; TASK 2
;; TASK 2
;; TASK 2

(define (roll-die)
   (define outcome (+ (random 6) 1))
   outcome
)

(define (roll-for-1)
   (define outcome (roll-die))
  (cond 
    [(equal? 1 outcome) (display outcome) (display " ")]
    [else (display outcome) (display " ") (roll-for-1)])
)

(define (roll-for-11)
  (define x (roll-for-1))
  (define y (roll-die))
  (cond 
    ((equal? 1 y) y)
    (else (display y) (display " ") (roll-for-11)))
  )

(define (prevprev a b)
  (define c (roll-die))
  (display c) (display " ")

  (cond    
    ((odd? a)
     
      (cond
        ((even? b)

         (cond
           ((even? c)            
            (prevprev b c)
            )

           ((odd? c)
            (display " ")
            )
           
           )
         
         )

        ((odd? b)
         (prevprev b c)
         )
        
        )
      
      )

    ((even? a)
     (prevprev b c)
     )
    
    )
  
  )


(define (roll-for-odd-even-odd)
  ( define a ( roll-die ) )
  ( display a ) ( display " " )
  ( define b ( roll-die ) )
  ( display b ) ( display " " )
  (prevprev a b)
  )


(define (roll-two-dice-for-a-lucky-pair)
  ( define a ( roll-die ) )
  ( define b ( roll-die ) )
  ( define sum (+ a b)) 
  ( display "(" ) ( display a ) ( display "," ) ( display b ) ( display ") ")
  
  ;;double, sum 7, 11
  (cond
    ((equal? 7 sum) (display "\n"))
    ((equal? 11 sum) (display "\n"))
    ((equal? a b) (display "\n"))
    (else (roll-two-dice-for-a-lucky-pair))
    )
  )



;; TASK 3
;; TASK 3
;; TASK 3

( define ( square n )
   (* n n)
   )
( define ( cube n )
   (* n n n)
   )
( define ( sequence name n )
   ( cond
      (( = n 1)
       ( display ( name 1 ) ) ( display " " )
       )
      ( else
        ( sequence name ( - n 1 ) )
        ( display ( name n ) ) ( display " " )
        )
      )
   )


(define (tri count sum n)
  (define s (+ sum count))

  (cond
    ((equal? count n) (display s))
    (else (define c (+ count 1)) (tri c s n))
    )
  
  )
  
(define (triangular n)  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;weird output "#<void>"
  (tri 1 0 n)
  )



(define (divisors n x s)

  (cond
    ((equal? x 0) (display s))
    (else
     (cond
       ((equal? 0 (modulo n x )) (define sum (+ s x)) (divisors n (- x 1) sum))
       (else (divisors n (- x 1) s))
       )
     )
    )
  )


(define (sigma n)
  (divisors n n 0)
  )

;;TASK 4
;;TASK 4
;;TASK 4

(define (row-of-dots n) 

  (define circle-big (circle 30 "solid" (rc)))
  (define circle-lil (circle 20 "solid" "white"))
  
   ( cond
      ((= n 0)
       empty-image
       )
      ((> n 0)
       (beside circle-big circle-lil (row-of-dots (- n 1)))
       )
      )
   )

(define (stack-of-rows n nn)

  (define circle-lil (circle 20 "solid" "white"))
  
  ( cond
     ((= n 0)
      empty-image
      )
     ((> n 0)
      (above (row-of-dots nn) circle-lil (stack-of-rows (- n 1) nn ))
      )
     )

  )
  

(define (hirst-dots n)

  (stack-of-rows n n)
  
  )



;;TASK 5
;;TASK 5
;;TASK 5

( define ( stella side count color1 color2 )
   ( define delta ( / side count ) )
   ( paint-nested-circles-two 1 count delta color1 color2 )
   )
( define ( paint-nested-circles-two from to delta color1 color2 )
   ( define side-length ( * from delta ) )
   ( cond
      ( ( = from to )
        ( if ( even? from )
             ( circle side-length "solid" color1 )
             ( circle side-length "solid" color2 )
             )
        )
      ( ( < from to )
        ( if ( even? from )
             ( overlay
               ( circle side-length "solid" color1 )
               ( paint-nested-circles-two ( + from 1 ) to delta color1 color2 )
               )
             ( overlay
               ( circle side-length "solid" color2 )
               ( paint-nested-circles-two ( + from 1 ) to delta color1 color2 )
               )
             )
        )
      )
   )


;;TASK 6
;;TASK 6
;;TASK 6

;;smtg wrong w the square function, getting all pissy abt wrong number of args

( define side-of-tile 100 )
( define diameter-of-pip ( * side-of-tile 0.2 ) )
( define radius-of-pip ( / diameter-of-pip 2 ) )
( define d ( * diameter-of-pip 1.4 ) )
( define nd ( * -1 d ) )
( define blank-tile ( rectangle side-of-tile side-of-tile "solid" "black" ) )    ;;change back to square
( define ( pip ) ( circle radius-of-pip "solid" "white" ) )
( define basic-tile1 ( overlay ( pip ) blank-tile ) )
( define basic-tile2
   ( overlay/offset ( pip ) d d
                    ( overlay/offset ( pip ) nd nd blank-tile
                                     )
                    )
   )
( define basic-tile3 ( overlay ( pip ) basic-tile2 ) )
( define basic-tile4
   ( overlay/offset ( pip ) d d
                    ( overlay/offset ( pip ) nd nd
                                     (overlay/offset (pip) nd d
                                                     (overlay/offset (pip) d nd blank-tile)
                                                     )
                                     )
                    )
   )
( define basic-tile5 ( overlay ( pip ) basic-tile4 ) )
( define basic-tile6 ( overlay/offset (pip) 0 d
                                      (overlay/offset (pip) 0 nd basic-tile4)
                                      )
   )

   
( define frame ( rectangle side-of-tile side-of-tile "outline" "gray" ) )      ;;change back to square
( define tile0 ( overlay frame blank-tile ) )
( define tile1 ( overlay frame basic-tile1 ) )
( define tile2 ( overlay frame basic-tile2 ) )
( define tile3 ( overlay frame basic-tile3 ) )
( define tile4 ( overlay frame basic-tile4 ) )
( define tile5 ( overlay frame basic-tile5 ) )
( define tile6 ( overlay frame basic-tile6 ) )
( define ( domino a b )
   ( beside ( tile a ) ( tile b ) )
   )
( define ( tile x )
   ( cond
      ( ( = x 0 ) tile0 )
      ( ( = x 1 ) tile1 )
      ( ( = x 2 ) tile2 )
      ( ( = x 3 ) tile3 )
      ( ( = x 4 ) tile4 )
      ( ( = x 5 ) tile5 )
      ( ( = x 6 ) tile6 )
      )
   )


;;TASK 7
;;TASK 7
;;TASK 7

(define (my-creation)

  (define r1 (rectangle 50 50 "solid" "gold"))
  (define c1 (circle 25 "solid" "gold"))
  (define r2 (rectangle 20 20 "solid" "gold"))
  (define c2 (circle 10 "solid" "midnight blue"))
  (define c3 (circle 10 "solid" "gold"))
  (define r3 (rectangle 60 40 "solid" "gold"))
  (define r4 (rectangle 60 20 "solid" "gold"))
  (define c4 (circle 10 "solid" "gold"))
  (define t1 (triangle 20 "solid" "gold"))
  (define c5 (circle 5 "solid" "midnight blue"))
  (define drop1 (overlay/offset t1 0 10 c4))
  (define wave (beside c2 r2 c2))
  (define w1 (flip-vertical (wedge 100 180 "solid" "midnight blue")))
  (define spark (rotate 45 (overlay/offset t1 0 10 c4)))
  (define r5 (rectangle 200 50 "solid" "white"))
  (define geh (text "г" 50 "black"))
  
  (define escutcheon (above (rectangle 200 30 "solid" "white") (rectangle 200 125 "solid" "midnight blue") w1))

  (define feather-tip (overlay (rotate 90 (wedge 10 90 "solid" "midnight blue")) (rectangle 10 10 "solid" "gold")))  
  (define fl1 (above (rectangle 10 40 "solid" "midnight blue") (rotate 90 (wedge 10 90 "solid" "gold")) (rectangle 10 20 "solid" "gold")))
  (define fl2 (above (rectangle 10 40 "solid" "midnight blue") feather-tip (rectangle 10 20 "solid" "midnight blue")))
  (define fl3 (above (rectangle 10 20 "solid" "midnight blue") (rotate 90 (wedge 10 90 "solid" "gold")) (rectangle 10 40 "solid" "gold")))
  (define fl4 (above (rectangle 10 20 "solid" "midnight blue") feather-tip (rectangle 10 40 "solid" "midnight blue")))
  (define fl5 (above (rotate 90 (wedge 10 90 "solid" "gold")) (rectangle 10 60 "solid" "gold")))
  (define fl6 (above feather-tip (rectangle 10 60 "solid" "midnight blue")))
  (define bottom-wing (beside (overlay/offset r1 0 25 c1) (rectangle 10 75 "solid" "midnight blue")))
  (define feathers (overlay/offset (flip-horizontal (right-triangle 45 25 "solid" "gold")) 5 -23 (beside fl1 fl2 fl3 fl4 fl5 fl6)))


  (define left-wing (above feathers bottom-wing ))
  (define right-wing (flip-horizontal left-wing))

  (define torso (overlay (above wave r4 wave) r3))
  (define neck (above c3 c3 c3 c3))
  (define torso-plus-neck (overlay neck torso))
  (define beak (flip-vertical (triangle 20 "solid" "gold")))
  (define beak-stack (above beak beak beak beak beak))
  (define torso-plus-neck-plus-beak-stack (overlay torso-plus-neck beak-stack))

  (define tail (overlay/offset c5 0 15 (overlay/offset drop1 0 10 (beside drop1 drop1))))
  
  (define body (above (rectangle 30 30 "solid" "midnight blue") (overlay/offset tail 0 50 torso-plus-neck-plus-beak-stack)))

  (define charge (beside right-wing body left-wing))  
  (define arms (overlay charge escutcheon))
  
  (display arms)
  
  )