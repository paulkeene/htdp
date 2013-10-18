#lang racket

; --- 6.1.1 ---
; 1. Evaluate by hand (distance-to-0 (make-posn 3 4))
; = sqrt (3^2 + 4^2)
; = sqrt (9 + 16)
; = sqrt (25)
; = 5
;
; 2. Evaluate by hand (distance-to-0 (make-posn (*2 3) (* 2 4)))
; = sqrt ((2*3)^2 + (2*4)^2)
; = sqrt (6^2 + 8^2)
; = sqrt (36 + 64)
; = sqrt (100)
; = 10
;
; 3. Evaluate by hand (distance-to-0 (make-posn 12 (- 6 1)))
; = sqrt (12^2 + (6-1)^2)
; = sqrt (12^2 + 5^2)
; = sqrt (144 + 25)
; = sqrt (169)
; = 13


; --- 6.2.2
; Note: these expressions are meant to be run in DrRacket
; dimensions of traffic light
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

; the positions of the bulbs
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

; draw the light with the red bulb turned on
(start WIDTH HEIGHT)
(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)

; get the y coordinate for the bulb with the given color
(define (get-bulb-y color)
  (cond
    [(symbol=? 'red color) Y-RED]
    [(symbol=? 'yellow color) Y-YELLOW]
    [(symbol=? 'green color) Y-GREEN]))

; clear the bulb with the given color
(define (clear-bulb color)
  (and
    (clear-solid-disk (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color)
    (draw-circle (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color)))


; --- 6.2.3 ---
; draw the bulb with the given color
(define (draw-bulb color)
  (draw-solid-disk (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color))


; --- 6.2.4 ---
(define (switch color-a color-b)
  (and (clear-bulb color-a) (draw-bulb color-b)))


; --- 6.2.5 ---
(define (next current-color)
  (cond
    [(and (symbol=? current-color 'red) (switch 'red 'green)) 'green]
    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red)) 'red]
    [(and (symbol=? current-color 'green) (switch 'green 'yellow)) 'yellow]))
