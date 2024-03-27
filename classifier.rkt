#lang racket
(require racket/trace)

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

(define (pick-two-cards)

  (define cards (deck))
  (define c1 (list-ref cards (random (length cards))))
  (define c2 (list-ref cards (random (length cards))))

  (cond
    (eq? c1 c2)
    (pick-two-cards)
    )

  (list c1 c2)
    
  )






(define (rank-to-num r)

  (cond
    ((eq? r 2)1)
    ((eq? r 3)2)
    ((eq? r 4)3)
    ((eq? r 5)4)
    ((eq? r 6)5)
    ((eq? r 7)6)
    ((eq? r 8)7)
    ((eq? r 9)8)
    ((eq? r 'X)9)
    ((eq? r 'J)10)
    ((eq? r 'Q)11)
    ((eq? r 'K)12)
    ((eq? r 'A)13)
    )
  
  )

(define (rank-to-text r)

  (cond
    ((eq? r 2)'two)
    ((eq? r 3)'three)
    ((eq? r 4)'four)
    ((eq? r 5)'five)
    ((eq? r 6)'six)
    ((eq? r 7)'seven)
    ((eq? r 8)'eight)
    ((eq? r 9)'nine)
    ((eq? r 'X) 'ten)
    ((eq? r 'J) 'jack)
    ((eq? r 'Q) 'queen)
    ((eq? r 'K) 'king)
    ((eq? r 'A) 'ace)
    )
  
  )



(define (higher-rank c1 c2)

  (define r1 (car c1))
  (define r2 (car c2))

  (define num1 (rank-to-num r1))
  (define num2 (rank-to-num r2))

  (define text1 (rank-to-text r1))
  (define text2 (rank-to-text r2))
  
  (cond
    ((> num1 num2)text1)
    ((> num2 num1)text2)
    ((eq? num1 num2) 'pair )
    )

  )


(define (classify-two-cards list)

  (display list)
  (display ':)
  (display " ")
  (display (higher-rank (car list) (second list)))
  (display " ")
  (display 'high)
  (display " ")

  (cond

    ((and (eq? (rank-to-num (car(car list))) ( - (rank-to-num (car(second list))) 1)) (eq? (second (car list)) (second (second list))))
     (display 'straight)
     (display " ")
     (display 'flush))
    ((and (eq? (rank-to-num (car(second list))) ( - (rank-to-num (car(car list))) 1)) (eq? (second (car list)) (second (second list))))
     (display 'straight)
     (display " ")
     (display 'flush))
    
    ((eq? (second (car list)) (second (second list)))
     (display 'flush))
    
    ((eq? (rank-to-num (car(car list))) ( - (rank-to-num (car(second list))) 1))
     (display 'straight))
    ((eq? (rank-to-num (car(second list))) ( - (rank-to-num (car(car list))) 1))
     (display 'straight))
    
    )

  )




(define (show card)

  (display (rank card))
  (display (suit card))

  )



( define (rank card)

   (car card)

   )

( define (suit card)

   (cadr card)

   )


(define (red? card)

  (or
   (equal? (suit card) 'D)
   (equal? (suit card) 'H)
   )

  )

(define (black? card)

  (not
   (red? card)
   )

  )

(define (aces? card1 card2)

  (and
   (equal? (rank card1) 'A)
   (equal? (rank card2) 'A)
   )


  )


;(trace higher-rank)