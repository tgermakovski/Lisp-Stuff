#lang racket

(require 2htdp/image)

(define (random-color) ( color (rgb-value) (rgb-value) (rgb-value) ) )
(define (rc) (random-color))
(define (rgb-value) ( random 256 ) )


(define (count-down n)
  (cond
    ((= n 0)(display " "))
    ((> n 0)
     (display n)
     (display "\n")
     (count-down (- n 1))
            )
        )
  )

(define (count-up n)
  (count-upp 1 n)
  )

(define (count-upp nn n)
  (cond
    ((> nn n)(display " "))
    ((< nn (+ n 1))
     (display nn)
     (display "\n")
     (count-upp (+ nn 1) n)
            )
        )
  )


(define (row-of-stars n)
  (cond
    ((= n 0)
     (display "\n")
            )
    ((> n 0)
     (display "* ")
     (row-of-stars (- n 1))
            )
        )
  )


(define (triangle-of-stars n)
  (tos 1 n)
  )

(define (tos nn n)
  (cond
    ((> nn n)(display " "))
    ((< nn (+ n 1))
     (row-of-stars nn)
     (tos (+ nn 1) n)
            )
        )
  )

(define (flip)
  (define x (random 0 2))
  (cond
    ((= x 0)"t")
    ((= x 1)"h")
        )
  )


(define (flip-for-difference n)
  (flip-for-dif 0 n (* n -1))
  )
  

(define (flip-for-dif dif n nn)

  (cond
    ((and (> dif nn) (< dif n))

       (define r (flip))
       (display "*")(display r)

     (cond
       ((eq? r "t")
        (define new-dif (- dif 1))
        (flip-for-dif new-dif n nn)
        )
       ((eq? r "h")
        (define new-dif (+ dif 1))
        (flip-for-dif new-dif n nn)
        )
      )
     
     )
    )
  )


(define (ccr n d)
  (define c (circle n "solid" (rc)))
  (ccrr c n d)
  )

(define (ccrr c n d)
  (define nn (- n d))
  (cond
    ((= n 0)
     (display c)
            )
    ((> n 0)
     (define newc (circle nn "solid" (rc)))
     (define combo (overlay newc c))
     (ccrr combo nn d)
            )
        )
  )



(define (cca n d color1 color2)
  (define c (circle n "solid" color1))
  (ccaa c n d color2 color1)
  )

(define (ccaa c n d color1 color2)
  (define nn (- n d))
  (cond
    ((= n 0)
     (display c)
            )
    ((> n 0)
     (define newc (circle nn "solid" color1))
     (define combo (overlay newc c))
     (ccaa combo nn d color2 color1)
            )
        )
  )

(define (ccs n d colors)
  (define len (length colors))
  (define nn (- n d))
  (define c (circle n "solid" (list-ref colors (random 0 len))))
  (ccss c len nn d colors)
  )

(define (ccss c len nn d colors)
  
  (cond
    ((= nn 0)(display c))
    ((> nn 0)
     (define cc (circle nn "solid" (list-ref colors (random 0 len))))
     (define combo (overlay cc c))
     (define nnn (- nn d))
     (ccss combo len nnn d colors)
            )
        )
  )






(define (cs r)
  (* r 2)
  )

(define (cc s)
  (define ss (/ s 2))
  (define sss (* ss ss))
  (define ssss (+ sss sss))
  (sqrt ssss)
  )

(define (ic s)
  (/ s 2)
  )

(define (is r)
  (define hyp (* r 2))
  (define hypp (* hyp hyp))
  (define ss (/ hypp 2))
  (sqrt ss)
  )

(define (cs-demo r c1 c2)
  (define s (cs r))
  (define sq (square s "solid" c1))
  (define cr (circle r "solid" c2))
  (overlay cr sq)
  )

(define (cc-demo s c1 c2)
  (define r (cc s))
  (define sq (square s "solid" c2))
  (define cr (circle r "solid" c1))
  (overlay sq cr)
  )

(define (ic-demo s c1 c2)
  (define r (ic s))
  (define sq (square s "solid" c2))
  (define cr (circle r "solid" c1))
  (overlay cr sq)
  )

(define (is-demo r c1 c2)
  (define s (is r))
  (define sq (square s "solid" c1))
  (define cr (circle r "solid" c2))
  (overlay sq cr)
  )

(define (image-1 s c1 c2)
  (define r (ic s))
  (define out (ic-demo s c1 c2))
  (define ss (is r))
  (define in (rotate 45 (ic-demo ss c1 c2)))
  (overlay in out)
  )

(define (image-2 s)
  (define out (square s "outline" "red"))
  (define ss (cc s))
  (define outt (rotate 45 (square ss "outline" "red")))
  (define sss (cc ss))
  (define outtt (square sss "outline" "red"))
  (define ssss (cc sss))
  (define outttt (rotate 45 (square ssss "outline" "red")))
  (overlay out outt outtt outttt)
  )

(define (warholesque-image s)
  (define gap (/ (- s 12) 2))
  (define horz (rectangle s 4 "solid" "black"))
  (define vert (rectangle 4 gap "solid" "black"))
  (define two (beside vert (image-1 gap (rc) (rc)) vert (image-1 gap (rc) (rc)) vert))
  (define twotwo (beside vert (image-1 gap (rc) (rc)) vert (image-1 gap (rc) (rc)) vert))
  (define four (above horz two horz twotwo horz))
  (display four)

  )


(define (tile a b c d)
  (define sq (square 100 "solid" a))
  (define out (circle 45 "solid" b))
  (define mid (circle 30 "solid" c))
  (define in (circle 15 "solid" d))
  (overlay in mid out sq)
  )


(define (dot-permutations a b c)
  (define c1 (overlay (circle 15 "solid" a) (circle 30 "solid" b) (circle 45 "solid" c)))
  (define c2 (overlay (circle 15 "solid" a) (circle 30 "solid" c) (circle 45 "solid" b)))
  (define c3 (overlay (circle 15 "solid" b) (circle 30 "solid" a) (circle 45 "solid" c)))
  (define c4 (overlay (circle 15 "solid" b) (circle 30 "solid" c) (circle 45 "solid" a)))
  (define c5 (overlay (circle 15 "solid" c) (circle 30 "solid" a) (circle 45 "solid" b)))
  (define c6 (overlay (circle 15 "solid" c) (circle 30 "solid" b) (circle 45 "solid" a)))
  (beside c1 c2 c3 c4 c5 c6)
  )