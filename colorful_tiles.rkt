#lang racket

(require 2htdp/image)

(define (random-color) ( color (rgb-value) (rgb-value) (rgb-value) ) )
(define (rc) (random-color))
(define (rgb-value) ( random 256 ) )


(define (types-of-tile)
  (display "standard-tile ")
  (display "random-color-tile ")
  (display "random-red-tile ")
  (display "random-green-tile ")
  (display "random-yellow-tile ")
  (display "random-cyan-tile ")
  (display "random-magenta-tile\n")
 )

(define (row-of-tiles n tile)
  (cond
    ((= n 0)
     empty-image
     )
    ((> n 0)
     (beside (row-of-tiles (- n 1) tile) (tile))
     )
    )
  )

(define (rectangle-of-tiles r c tile)
  (cond
    ((= r 0)
     empty-image
     )
    ((> r 0)
     (above (rectangle-of-tiles (- r 1) c tile) (row-of-tiles c tile))
     )
   )
  )

(define (square-of-tiles n tile)
  (rectangle-of-tiles n n tile)
  )

(define (standard-tile) (square 40 "outline" "black"))

(define (random-color-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (rc))
   )
  )

(define (random-blue-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-blue-color))
   )
  )

(define (random-red-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-red-color))
   )
  )

(define (random-green-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-green-color))
   )
  )

(define (random-yellow-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-yellow-color))
   )
  )

(define (random-magenta-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-magenta-color))
   )
  )

(define (random-cyan-tile)
  (overlay
   (square 40 "outline" "black")(square 40 "solid" (random-cyan-color))
   )
  )



(define (dot-tile)
  (overlay
   (circle 35 "solid" (rc))(square 100 "solid" "white")
   )
  )

(define (random-blue-dot-tile)
  (overlay
   (circle 35 "solid" (random-blue-color))(square 100 "solid" "white")
   )
  )

(define (random-red-dot-tile)
  (overlay
   (circle 35 "solid" (random-red-color))(square 100 "solid" "white")
   )
  )

(define (random-green-dot-tile)
  (overlay
   (circle 35 "solid" (random-green-color))(square 100 "solid" "white")
   )
  )

(define (random-yellow-dot-tile)
  (overlay
   (circle 35 "solid" (random-yellow-color))(square 100 "solid" "white")
   )
  )

(define (random-magenta-dot-tile)
  (overlay
   (circle 35 "solid" (random-magenta-color))(square 100 "solid" "white")
   )
  )

(define (random-cyan-dot-tile)
  (overlay
   (circle 35 "solid" (random-cyan-color))(square 100 "solid" "white")
   )
  )


(define (random-blue-color) (color 0 0 255 (dark-rgb-value)))
(define (random-red-color) (color 255 0 0 (dark-rgb-value)))
(define (random-green-color) (color 0 255 0 (dark-rgb-value)))
(define (random-yellow-color) (color 255 255 0 (dark-rgb-value)))
(define (random-magenta-color) (color 255 0 255 (dark-rgb-value)))
(define (random-cyan-color) (color 0 255 255 (dark-rgb-value)))

(define (dark-rgb-value) (+ (random 128) 128))



(define (ccs-tile)
  (define a (random-color))(define b (random-color))(define c (random-color))
  (define x (list a b c))
  (define circ (ccs 35 7 x))
  (overlay circ (square 100 "solid" "white"))
  )


(define (ccs n d colors)
  (define len (length colors))
  (define nn (- n d))
  (define c (circle n "solid" (list-ref colors (random 0 len))))
  (ccss c len nn d colors)
  )

(define (ccss c len nn d colors)
  
  (cond
    ((or (< nn 0) (= nn 0)) c)
    ((> nn 0)
     (define cc (circle nn "solid" (list-ref colors (random 0 len))))
     (define combo (overlay cc c))
     (define nnn (- nn d))
     (ccss combo len nnn d colors)
            )
        )
  )

(define (anim n)
  (define s (rectangle 1500 500 "solid" "green"))
  (define ss (rectangle 1500 n "solid" "blue"))
  (define sss (above s ss))
  (cond
    ((< 0 n)
     (display sss)(anim (- n 1))     
     )
  ))

(define (anim2 n nn)
  (define c (circle 25 "solid" "blue"))
  (define r (rectangle n 1 "solid" "white"))
  (define rr (rectangle (- nn n) 1 "solid" "white"))
  (define crcrr (beside c r c rr))
  (cond
    ((< 0 n)
     (display crcrr)(anim2 (- n 1) nn)     
     )
  ))







(define (diamond-tile)

  (define a (random-color))
  (define back (square 100 "solid" "white"))
  (define big (square 70 "solid" a))
  (define mid (square 60 "solid" "white"))
  (define smol (square 50 "solid" a))
  (define tiny (square 40 "solid" "white"))
  (rotate 45 (overlay tiny smol mid big back))

  )

(define (wild-square-tile)

  (define a (random-color))
  (define back (square 100 "solid" "white"))
  (define big (square 70 "solid" a))
  (define mid (square 60 "solid" "white"))
  (define smol (square 50 "solid" a))
  (define tiny (square 40 "solid" "white"))
  (define angle (random 0 360))
  (rotate angle (overlay tiny smol mid big back))

  )



(define (sampler)
  (display "(?): ")
  (define the-list (read))
  (define the-element
    (list-ref the-list (random (length the-list )))
    )
  (display the-element)(display "\n")(sampler)
  )

(define (ranks rank)
  (list
   (list rank 'C)
   (list rank 'D)
   (list rank 'H)
   (list rank 'S)
   )
  )

(define (deck)
  (append
   (ranks 2)
   (ranks 3)
   (ranks 4)
   (ranks 5)
   (ranks 6)
   (ranks 7)
   (ranks 8)
   (ranks 9)
   (ranks 'X)
   (ranks 'J)
   (ranks 'Q)
   (ranks 'K)
   (ranks 'A)
   )
  )

(define (pick-a-card)
  (define cards (deck))
  (list-ref cards (random (length cards)))
  )

(define (show card)
  (display (rank card))
  (display (suit card))
  )

(define (rank card)(car card))
(define (suit card)(cadr card))
(define (red? card)(or(equal? (suit card)'D)
                      (equal? (suit card)'H)
                      ))
(define (black? card)(not (red? card)))

(define (aces? card1 card2)
  (and
   (equal? (rank card1)'A)
   (equal? (rank card2) 'A)))






