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
