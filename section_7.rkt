#lang racket

; --- 7.1.1 ---
; (number? (make-posn 2 3) -> False
; (number? (+ 12 10)) -> True
; (posn? 23) -> False
; (posn? (make-posn 23 3)) -> True
; (star? (make-posn 23 3)) -> False


; --- 7.1.2 ---
(define-struct posn (x y))
(define-struct circle (center radius))
(define-struct square (nw length))
(define PI 3.14159)

(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* (square-length a-shape) 4)]
    [(circle? a-shape) (* 2 PI (circle-radius a-shape))]))

(define my-square (make-square (make-posn 0 0) 5))
(define my-circle (make-circle (make-posn 0 0) 3))

(= (perimeter my-square) 20)
(= (perimeter my-circle) (* 2 PI 3))


; --- 7.1.3 ---
(define (area a-shape)
  (cond
    [(square? a-shape) (sqr (square-length a-shape))]
    [(circle? a-shape) (* PI (sqr (circle-radius a-shape)))]))

(= (area my-square) 25)
(= (area my-circle) (* PI 9))
