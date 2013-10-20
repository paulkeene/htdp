#lang racket

(define-struct posn (x y))

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
;(start WIDTH HEIGHT)
;(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
;(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
;(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)

; get the y coordinate for the bulb with the given color
(define (get-bulb-y color)
  (cond
    [(symbol=? 'red color) Y-RED]
    [(symbol=? 'yellow color) Y-YELLOW]
    [(symbol=? 'green color) Y-GREEN]))

; clear the bulb with the given color
;(define (clear-bulb color)
;  (and
;    (clear-solid-disk (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color)
;    (draw-circle (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color)))


; --- 6.2.3 ---
; draw the bulb with the given color
;(define (draw-bulb color)
;  (draw-solid-disk (make-posn X-BULBS (get-bulb-y color)) BULB-RADIUS color))


; --- 6.2.4 ---
;(define (switch color-a color-b)
;  (and (clear-bulb color-a) (draw-bulb color-b)))


; --- 6.2.5 ---
;(define (next current-color)
;  (cond
;    [(and (symbol=? current-color 'red) (switch 'red 'green)) 'green]
;    [(and (symbol=? current-color 'yellow) (switch 'yellow 'red)) 'red]
;    [(and (symbol=? current-color 'green) (switch 'green 'yellow)) 'yellow]))


; --- 6.3.1 ---
; movie
; constructor: make-movie
; selectors: movie-title, movie-producer
;
; boyfriend
; constructor: make-boyfriend
; selectors: boyfriend-name, boyfriend-hair, boyfriend-eyes boyfriend-phone
;
; cheerleader
; constructor: make-cheerleader
; selectors: cheerleader-name, cheerleader-numbers
;
; CD
; constructor: make-CD
; selectors: CD-artist, CD-title, CD-price
;
; sweater:
; constructor: make-sweater
; selectors: sweater-material, sweater-size, sweater_producer


; --- 6.3.2 ---
(define-struct movie (title producer))

(movie-title (make-movie 'TheEmpireStrikesBack 'Lucas))
(movie-producer (make-movie 'TheEmpireStrikesBack 'Lucas))

; movie-title :: movie -> sybmol
; movie-producer :: movie -> symbol
; make-movie :: symbol -> symbol -> movie

(define-struct jet-fighter (designation acceleration top-speed range))

(define (within-range fighter distance)
  (<= distance (jet-fighter-range fighter)))

(define f22 (make-jet-fighter 'f22 100.0 500.0 1000.0))
(equal? (within-range f22 950.0) #t)
(equal? (within-range f22 2250.0) #f)

(define (reduce-range fighter)
  (make-jet-fighter
    (jet-fighter-designation fighter)
    (jet-fighter-acceleration fighter)
    (jet-fighter-top-speed fighter)
    (* .8 (jet-fighter-range fighter))))

(= 800.0 (jet-fighter-range (reduce-range f22)))


; --- 6.4.1 ---
; A movie is a structure:
;   (make-movie title producer)
; where title and producer are symbols
;
; A boyfriend is a structure:
;   (make-boyfriend name hair eyes phone)
; where name, hair, eyes, and phone are symbols
;
; A cheerleader is a structure:
;   (make-cheerleader name number)
; where name is a symbol and number is a number (integer)
;
; A CD is a structure:
;   (make-CD artist title price)
; where artist and title are symbols and price is a float
;
; A sweater is a structure:
;   (make-sweater material size producer)
; where material size and producer are symbols


; --- 6.4.2 ---
; A time-since-midnight object is a structure:
;   (make-time-since-midnight hours minutes seconds)
; where hours minutes and seconds are numbers (integers)

(define-struct time-since-midnight (hours minutes seconds))

; A 3-letter-word is a structure:
;   (make-three-letter-word first second third)
; where first, second, and third are symbols

(define-struct three-letter-word (first second third))


; --- 6.5.1 ---
; func : movie -> ???
; (define (func a-movie)
;   ... (movie-title a-movie) ...
;   ... (movie-producer a-movie) ...
;
; func : boyfriend -> ???
; (define (func a-boyfriend)
;   ... (boyfriend-name a-boyfriend) ...
;   ... (boyfriend-hair a-boyfriend) ...
;   ... (boyfriend-eyes a-boyfriend) ...
;   ... (boyfriend-phone a-boyfriend) ...
;
; func : cheerleader -> ???
; (define (func a-cheerleader)
;   ... (cheerleader-name a-cheerleader) ...
;   ... (cheerleader-number a-cheerleader) ...
;
; func : CD -> ???
; (define (func a-CD)
;   ... (CD-artist a-CD) ...
;   ... (CD-title a-CD) ...
;   ... (CD-price a-CD) ...
;
; func : sweater -> ???
; (define (func a-sweater)
;   ... (sweater-material a-sweater) ...
;   ... (sweater-size a-sweater) ...
;   ... (sweater-producer a-sweater) ...


; --- 6.5.2 ---
; seconds-since-midnight : time-since-midnight -> number;
; (define (seconds-since-midnight a-time)
;   ... (time-since-midnight-hours a-time) ...
;   ... (time-since-midnight-minutes a-time) ...
;   ... (time-since-midnight-seconds a-time) ...

(define (seconds-since-midnight a-time)
  (+
    (time-since-midnight-seconds a-time)
    (* 60 (time-since-midnight-minutes a-time))
    (* 60 60 (time-since-midnight-hours a-time))))

(define my-time (time-since-midnight 12 30 2))
(= 45002 (seconds-since-midnight my-time))


; --- 6.6.1 ---
; fun-for-circle : circle -> ???
; (define (fun-for-circle a-circle)
;   ... (circle-center a-circle) ...
;   ... (circle-center a-radius) ...
;   ... (circle-center a-color) ...

(define-struct circle (center radius color))


; --- 6.6.2 ---
(define (draw-a-circle a-circle)
  (draw-circle
    (circle-center a-circle)
    (circle-radius a-circle)
    (circle-color a-circle)))
;
;(define my-circle (make-circle (make-posn 150 200) 100 'blue))
;
; test code
;(start 300 300)
;(draw-a-circle my-circle)


; --- 6.6.3 ---
(define (distance-to-center a-circle a-posn)
  (+
    (sqr (- (posn-x a-posn) (posn-x (circle-center a-circle))))
    (sqr (- (posn-y a-posn) (posn-y (circle-center a-circle))))))

(define (in-circle? a-circle a-posn)
  (<= (distance-to-center a-circle a-posn) (circle-radius a-circle)))

(define my-circle (make-circle (make-posn 6 2) 1 'green))

(equal? (in-circle? my-circle (make-posn 6 1.5)) #t)
(equal? (in-circle? my-circle (make-posn 8 6)) #f)


; --- 6.6.4 ---
(define (translate-circle a-circle delta)
  (make-circle
    (make-posn
      (+ (posn-x (circle-center a-circle)) delta)
      (posn-y (circle-center a-circle)))
    (circle-radius a-circle)
    (circle-color a-circle)))

(= (posn-x (circle-center (translate-circle my-circle 3))) 9)
(= (posn-y (circle-center (translate-circle my-circle 3))) 2)


; --- 6.6.5 ---
;(define (clear-a-circle a-circle)
;  (clear-circle
;    (circle-center a-circle)
;    (circle-radius a-circle)
;    (circle-color a-circle)))


; --- 6.6.6 ---
;(define (draw-and-clear-circle a-circle sleep-seconds)
;  (and
;    (draw-a-circle a-circle)
;    (sleep-for-a-while sleep-seconds)
;    (clear-a-circle a-circle)))
;
;(define (move-circle delta a-circle)
;  (cond
;    [(draw-and-clear-circle a-circle 1) (translate-circle a-circle delta)]
;    [else a-circle]))
;
; test code
;(start 200 100)
;(draw-a-circle
;  (move-circle 10
;    (move-circle 10
;      (move-circle 10
;        (move-circle 10 my-circle)


; --- 6.6.7 ---
; A rectangle is a structure:
;   (make-rectangle corner height width color)
; where corner is a posn, height and width are numbers and color is a symbol

(define-struct rectangle (corner width height color))

; fun-for-rect : rectangle -> ???
; (define (func a-rect)
;   ... (rectangle-corner a-rect) ...
;   ... (rectangle-height a-rect) ...
;   ... (rectangle-width a-rect) ...
;   ... (rectangle-color a-rect) ...


; --- 6.6.8 ---
;(define (draw-a-rectangle a-rectangle)
;  (draw-solid-rect
;    (rectangle-corner a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)
;    (rectangle-color a-rectangle)))

(define my-rect (make-rectangle (make-posn 100 100) 50 125 'red))


; --- 6.6.9 ---
(define (in-rectangle? a-rectangle a-posn)
  (and
    (and
      (> (posn-x a-posn) (posn-x (rectangle-corner a-rectangle)))
      (<= (- (posn-x a-posn) (posn-x (rectangle-corner a-rectangle)))
          (rectangle-width a-rectangle)))
    (and
      (> (posn-y a-posn) (posn-y (rectangle-corner a-rectangle)))
      (<= (- (posn-y a-posn) (posn-y (rectangle-corner a-rectangle)))
          (rectangle-height a-rectangle)))))

(equal? (in-rectangle? my-rect (make-posn 101 101)) #t)
(equal? (in-rectangle? my-rect (make-posn 149 224)) #t)
(equal? (in-rectangle? my-rect (make-posn 50 30)) #f)
(equal? (in-rectangle? my-rect (make-posn 125 250)) #f)


; --- 6.6.10 ---
(define (translate-rectangle a-rectangle delta)
  (make-rectangle
    (make-posn (+ (posn-x (rectangle-corner a-rectangle)) delta)
               (posn-y (rectangle-corner a-rectangle)))
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)
    (rectangle-color a-rectangle)))


; --- 6.6.11 ---
;(define (clear-a-rectangle a-rectangle)
;  (clear-solid-rect
;    (rectangle-corner a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)))


; --- 6.6.12 ---
;(define (move-rectangle delta a-rectangle)
;  (cond
;    [(draw-and-clear-rectangle a-rectangle 1)
;     (translate-rectangle a-rectangle delta)]
;    [else a-rectangle]))

;(define (draw-and-clear-rectangle a-rectangle sleep-seconds)
;  (and
;    (draw-a-rectangle a-rectangle)
;    (sleep-for-a-while sleep-seconds)
;    (clear-a-rectangle a-rectangle)))

; test code
;(start 300 300)
;(draw-a-rectangle
;  (move-rectangle 10
;    (move-rectangle 10
;      (move-rectangle 10
;        (move-rectangle 10 my-rectangle)))))


; --- 6.7.1 ---
(define HEAD (make-circle (make-posn 150 100) 50 'black))
(define BODY_START (make-posn 150 150))
(define BODY_END (make-posn 150 250))
(define ARMS_START (make-posn 150 175))
(define LEFT_ARM_END (make-posn 100 150))
(define RIGHT_ARM_END (make-posn 200 150))
(define LEGS_START (make-posn 150 250))
(define LEFT_LEG_END (make-posn 75 325))
(define RIGHT_LEG_END (make-posn 225 325))
(define CROSS_BEAM_START (make-posn 0 5))
(define CROSS_BEAM_END (make-posn 150 5))
(define ROPE_START (make-posn 150 5))
(define ROPE_END (make-posn 150 50))

(define (draw-next-part piece)
  (cond
    [(symbol=? 'head piece) (draw-a-circle HEAD)]
    [(symbol=? 'body piece)
      (draw-solid-line BODY_START BODY_END 'black)]
    [(symbol=? 'left-arm piece)
      (draw-solid-line ARMS_START LEFT_ARM_END 'black)]
    [(symbol=? 'right-arm piece)
      (draw-solid-line ARMS_START RIGHT_ARM_END 'black)]
    [(symbol=? 'left-leg piece)
      (draw-solid-line LEGS_START LEFT_LEG_END 'black)]
    [(symbol=? 'right-leg piece)
      (draw-solid-line LEGS_START RIGHT_LEG_END 'black)]
    [(symbol=? 'noose piece)
      (and
        (draw-solid-line CROSS_BEAM_START CROSS_BEAM_END 'black)
        (draw-solid-line ROPE_START ROPE_END 'black))]))

; test code
;(start 500 500)
;(draw-next-part 'noose)
;(draw-next-part 'head)
;(draw-next-part 'body)
;(draw-next-part 'left-arm)
;(draw-next-part 'right-arm)
;(draw-next-part 'left-leg)
;(draw-next-part 'right-leg)


; --- 6.7.2 ---
; A three-letter-word is a structure:
;   (make-three-letter-word first second third)
; where first, second, and third are symbols
;(define-struct three-letter-word (first second third))


; --- 6.7.3 ---
(define (reveal chosen status letter)
  (make-three-letter-word
    (cond
      [(symbol=? letter (three-letter-word-first chosen)) letter]
      [else (three-letter-word-first status)])
    (cond
      [(symbol=? letter (three-letter-word-second chosen)) letter]
      [else (three-letter-word-second status)])
    (cond
      [(symbol=? letter (three-letter-word-third chosen)) letter]
      [else (three-letter-word-third status)])))

(define my-chosen (make-three-letter-word 'c 'a 't))
(define starting-status (make-three-letter-word '_ '_ '_))

(symbol=?
  (three-letter-word-first (reveal my-chosen starting-status 'c))
  'c)
(symbol=?
  (three-letter-word-second (reveal my-chosen starting-status 'a))
  'a)
(symbol=?
  (three-letter-word-third (reveal my-chosen starting-status 't))
  't)
