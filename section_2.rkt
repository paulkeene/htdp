#lang racket

; --- 2.1.1 ---
; Square a number
(expt 5 2)

; Compute the sine of an angle
(sin 3.14159)

; Find the max of two numbers
(max 4 22)


; --- 2.1.2 ---
; Play with square root and tan
(sqrt 4)
(sqrt 2)
(sqrt -1)
(tan 3.14159)


; --- 2.2.1 ---
; Convert a temperature in degrees Fahrenheit to degrees Celsius
(define (Fahrenheit->Celsius degrees-fahrenheit)
  (/ (* (- degrees-fahrenheit 32) 5) 9))

(= (Fahrenheit->Celsius 100) 340/9)
(= (Fahrenheit->Celsius 32) 0)
(= (Fahrenheit->Celsius 66) 170/9)


; --- 2.2.2 ---
(define (dollar->euro dollars)
  (* 0.74 dollars))

(= (dollar->euro 0) 0)
(= (dollar->euro 100) 74)
(= (dollar->euro 26.25) 19.425)


; --- 2.2.3 ---
(define (triangle base height)
    (/ (* base height) 2.0))

(= (triangle 15.0 11.3) 84.75)
(= (triangle 3 20) 30)
(= (triangle 1 2) 1)


; --- 2.2.4 ---
(define (convert3 a b c)
  (+
    (+ (* b 10) a)
    (* c 100)
  ))

(= (convert3 1 2 3) 321)
(= (convert3 1 0 0) 1)
(= (convert3 6 0 7) 706)


; --- 2.2.5 ---
(define (f n)
  (+ (* n n) 10))

(f 3)
(f 4)

(define (g n)
  (+ 20 (* (/ 1 2) (* n n))))

(g 3)
(g 4)

(define (h n)
  (- 2 (/ 1 n)))
(h 3)
(h 4)


; --- 2.3.1 ---
(define (tax gross-wage)
  (* 0.15 gross-wage))

(tax 100)
(tax 8)

(define (wage hours-worked)
  (* 12 hours-worked))

(wage 100)
(wage 8)

(define (netpay hours-worked)
  (- (wage hours-worked)
     (tax (wage hours-worked))))

(netpay 100)
(netpay 8)


; --- 2.3.2 ---
(define (sum-coins pennies nickels dimes quarters)
  (+ (+ (+ (* 0.01 pennies) (* nickels 0.05)) (* 0.1 dimes)) (* 0.25 quarters)))

(sum-coins 32 2 1 4)


; --- 2.3.3 ---
(define (total-profit attendees)
  (- (* 5 attendees)
     (+ 20
        (* 0.5 attendees))))

(total-profit 100)
(total-profit 4)


; --- 2.4.1 ---
; (+ (10) 20)
; (10 + 20)
; (+ +)


; --- 2.4.2 ---
; (define (f2 1)
;   (+ x 10))
(define (f2 x)
  (+ x 10))

; (define (g2 x)
;   + x 10)
(define (g2 x)
  (+ x 10))

; (define h2(x)
;   (+ x 10))
(define (h2 x)
  (+ x 10))


; --- 2.4.3 ---
; (+ 5 (/ 1 0))
; (sin 10 20)
; (somef 10)


; --- 2.4.4 ---
(define (somef x)
  (sin x x))
;(somef 10 20)
;(somef 10)
