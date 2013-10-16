#lang racket

; --- 4.1.1 ---
(equal? (and (> 4 3) (<= 10 100)) #t)
(equal? (or (> 4 3) (= 10 100)) #t)
(equal? (not (= 2 3)) #t)


; --- 4.1.2 ---
(equal? (> 4 3) #t)
(equal? (and (> 4 4) (> 4 3)) #f)
(equal? (= (* 4 4) 4) #f)


; --- 4.2.1 ---
(define (func-a n)
  (and (> n 3)
       (<= n 7)))

(equal? (func-a 4) #t)
(equal? (func-a 3) #f)
(equal? (func-a 7) #t)

(define (func-b n)
  (and (>= n 3)
       (<= n 7)))

(equal? (func-b 4) #t)
(equal? (func-b 3) #t)
(equal? (func-b 7) #t)

(define (func-c n)
  (and (>= n 3)
       (< n 9)))

(equal? (func-c 3) #t)
(equal? (func-c 5) #t)
(equal? (func-c 9) #f)

(define (func-d n)
  (or
      (and (> n 1)
           (< n 3))
      (and (> n 9)
           (< n 11))))

(equal? (func-d 2) #t)
(equal? (func-d 10) #t)
(equal? (func-d 1) #f)
(equal? (func-d 3) #f)
(equal? (func-d 9) #f)
(equal? (func-d 11) #f)


; --- 4.2.2 ---
; 1. <= -5 -4 -3 -2 -1  0  1  2  3  4 =>
;              (========)
;
; 2. <= -1  0  1  2  3  4  5 =>
;    <=========)  (===========>
;
; 3. <=  0  1  2  3  4  5  6 =>
;    <======)           (=====>
;
; in-interval-1? : number -> boolean
; to determine whether x is less than -3
;
; in- interval-2? : number -> boolean
; to determine whether x is less than 1 or greater than 2
;
; in-interval-3? : number -> boolean
; to determine whether x is between 1 and 5 inclusively
;
; (in-interval-1? -2) :: True ; (and True True)
;
; (in-interval-2? -2) :: True ; (or True False)
;
; (in-interval-3? -2) :: True ; (not (and False True))


; --- 4.2.3 ---

(define (eqn-1 n)
  (= (+ (* 4 n) 2) 62))

(equal? (eqn-1 15) #t)
(equal? (eqn-1 10) #f)
(equal? (eqn-1 12) #f)
(equal? (eqn-1 14) #f)

(define (eqn-2 n)
  (= (* (* n n) 2) 102))

(equal? (eqn-2 10) #f)
(equal? (eqn-2 12) #f)
(equal? (eqn-2 14) #f)

(define (eqn-3 n)
  (= (+ (+ (* (* n n) 4) (* 6 n)) 2) 462))

(equal? (eqn-3 10) #t)
(equal? (eqn-3 12) #f)
(equal? (eqn-3 14) #f)


; --- 4.2.4 ---
; Added tests to section_2.rkt


; --- 4.3.1 ---
; (cond
;   [< n 10) 20]
;   [(and (> n 20) (<= n 30))]
;   [else 1])
; This expression is illegal since the second clause doesn't
; contain an answer expression


; --- 4.3.2 ---
; a) .040
; b) .045
; c) .060


; --- 4.3.3 ---
; a) 40
; b) 121
; c) 495


; --- 4.4.1 ---
(define (interest deposit-amount)
  (* deposit-amount
     (cond
       [(<= deposit-amount 1000) .04]
       [(<= deposit-amount 5000) .045]
       [else .05])))

(= (* .04 1000) (interest 1000))
(= (* .045 5000) (interest 5000))
(= (* .05 7000) (interest 7000))


; --- 4.4.2 ---
(define (tax income)
  (* income
     (cond
       [(<= income 240) 0]
       [(<= income 480) .15]
       [else .28])))

(= 0 (tax 240))
(= (* .15 480) (tax 480))
(= (* .28 500) (tax 500))

(define (grosspay hours-worked)
  (* hours-worked 12))

(= 0 (grosspay 0))
(= 120 (grosspay 10))

(define (netpay hours-worked)
    (- (grosspay hours-worked)
       (tax (grosspay hours-worked))))

(= 408 (netpay 40))
(= 691.2 (netpay 80))


; --- 4.4.3 ---
; If a customer charges $2000 then they'll receive
; (.0025 * $500) + (.0050 * $1000) + (.0075 * $500) = $10
;
; If a customer charges $2600 then they'll receive
; (.0025 * $500) + (.0050 * $1000) + (.0075 * $1000) + (.01 * $100) = $14.75

(define (tier1-rewards charge-amount)
  (* .0025 (min charge-amount 500)))

(= (* .0025 500) (tier1-rewards 500))

(define (tier2-rewards charge-amount)
  (cond
    [(<= charge-amount 500) 0]
    [else (* .0050 (min (- charge-amount 500) 1000))]))

(= (* .0050 1000) (tier2-rewards 1500))
(= 0 (tier2-rewards 500))

(define (tier3-rewards charge-amount)
  (cond
    [(<= charge-amount 1500) 0]
    [else (* .0075 (min (- charge-amount 1500) 1000))]))

(= (* .0075 1000) (tier3-rewards 2500))
(= 0 (tier3-rewards 1500))

(define (tier4-rewards charge-amount)
  (cond
    [(<= charge-amount 2500) 0]
    [else (* .01 (- charge-amount 2500))]))

(= (* .01 2000) (tier4-rewards 4500))
(= 0 (tier4-rewards 2500))

(define (pay-back charge-amount)
  (+ (tier1-rewards charge-amount) (tier2-rewards charge-amount)
     (tier3-rewards charge-amount) (tier4-rewards charge-amount)))

(= 10 (pay-back 2000))
(= 14.75 (pay-back 2600))


; --- 4.4.4 ---
(define (how-many a b c)
  (cond
    [(> (* b b) (* 4 a c)) 2]
    [(= (* b b) (* 4 a c)) 1]
    [else 0]))

(= 2 (how-many 1 0 -1))
(= 1 (how-many 2 4 2))
(= 2 (how-many 3 5 0))
(= 0 (how-many 4 0 2))

; If we were no longer guaranteed that the equation was proper
; we would need to add a conditional clause at the beginning that
; checks to see if a is equal to 0 and, if so, return 0.
