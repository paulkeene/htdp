#lang racket

(define-struct posn (x y) #:transparent)

; --- 11.2.1 ---
; repeat : number symbol -> list-of-symbols
; Construct a list of the specified length where every element is
; the specified symbol
(define (repeat n sym)
  (cond
    [(zero? n) empty]
    [else (cons sym (repeat (sub1 n) sym))]))

(equal? (cons 'cat (cons 'cat (cons 'cat (cons 'cat (cons 'cat empty)))))
        (repeat 5 'cat))
(equal? empty (repeat 0 'dog))


; --- 11.2.2 ---
; f : number -> number
; Computes 3x^2 -6x -1
(define (f x)
  (+ (* 3 (* x x))
     (+ (* -6 x)
        -1)))

; tabulate : number -> list-of-posns
; Constructs a list of posns where the posns are of the form
; [(n, f(n)) (n-1, f(n-1)) ... (1, f(1)]. The resulting list will contain
; n elements.
(define (tabulate n)
  (cond
    [(zero? n) empty]
    [else (cons (make-posn n (f n))
                (tabulate (sub1 n)))]))

(equal? empty (tabulate 0))
(equal? (cons (make-posn 3 8)
        (cons (make-posn 2 -1)
        (cons (make-posn 1 -4)
        empty)))
        (tabulate 3))


; --- 11.2.3 ---
; apply-n-helper : number -> list-of-shapes
; Applies move-picture n times to FACE
;(define (apply-n n)
;  (cond
;    [(zero? n) FACE]
;    [else (move-picture 1 (apply-n (sub1 n)))]))


; --- 11.2.4 ---
; depth : a-deep-list -> number
; Computes the number of cons operators used to construct a-deep-list
(define (depth a-deep-list)
  (cond
    [(symbol? a-deep-list) 0]
    [else (+ (depth (first a-deep-list)) 1)]))

(= 0 (depth 'a))
(= 1 (depth (cons 'a empty)))
(= 4 (depth (cons (cons (cons (cons 'a empty) empty) empty) empty)))

; make-deep : symbol number -> deep-list
; Constructs a deep list using the specified number of conses and the
; specified symbol.
(define (make-deep s n)
  (cond
    [(zero? n) s]
    [else (cons (make-deep s (sub1 n)) empty)]))

(equal? 'a (make-deep 'a 0))
(equal? (cons 'a empty) (make-deep 'a 1))
(equal? (cons (cons (cons (cons 'a empty) empty) empty) empty)
        (make-deep 'a 4))


; random-n-m : integer integer -> integer
; Computes a random integer greater than or equal to n and less than m.
; Assume n < m.
(define (random-n-m n m)
  (+ (random (- m n)) n))

; Example
; (random-n-m 4 10)
; = (+ (random (- 10 4)) 4)
; = (+ (random 6) 4)
;
; If we look at the documentation for random we see that this function
; returns an integer between 0 and n-1 inclusive. This means that
; (random 6) will return us an integer between 0 and 5 inclusive. Since
; we add this number to 4 that means (random-n-m 4 10) will return us an
; integer between 4 and 9. This shows that random-n-m computers a random
; number greater than or equal to n and less than m.
;
; <=  2  3  4  5  6  7  8  9  10  11  =>
;           [==============]


; --- 11.3.2 ---
; tie-dyed : number -> list-of-numbers
; Returns a list of n randomly chosen integers in the range 20 to 120.
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m 20 120) (tie-dyed (sub1 n)))]))

(= 0 (length (tie-dyed 0)))
(= 4 (length (tie-dyed 4)))

; The following definitions and expressions are meant to be executed in
; DrRacket with the draw.rkt teachpack enabled.

;(define (draw-circles a-posn radii color)
;  (cond
;    [(empty? radii) true]
;    [else (and
;            (draw-circle a-posn (first radii) color)
;            (draw-circles a-posn (rest radii) color))]))

;(start 120 120)
;(draw-circles (make-posn 80 49) (tie-dyed 2) 'green)
;(draw-circles (make-posn 100 42) (tie-dyed 3) 'purple)
;(draw-circles (make-posn 15 80) (tie-dyed 4) 'yellow)
;(draw-circles (make-posn 100 100) (tie-dyed 2) 'blue)


; --- 11.3.3 ---
; create-temps : number number number -> list of integers
; Creates a list of n numbers where each number is between the lower
; and upper bounds.
(define (create-temps n lower upper)
  (cond
    [(zero? n) empty]
    [else (cons (random-n-m lower upper)
                (create-temps (sub1 n) lower upper))]))

(= 0 (length (create-temps 0 55 234)))
(= 4 (length (create-temps 4 55 234)))

; check-range1 : list-of-temperatures -> boolean
; to determine if all the temperatures in the passed-in list are between
; 5 and 95 degrees Celsius.
(define (check-range1 temperatures)
  (cond
    [(empty? temperatures) true]
    [else (and
      (and (< (first temperatures) 95) (> (first temperatures) 5))
      (check-range1 (rest temperatures)))]))

(equal? #t (check-range1 (create-temps 100 6 95)))
(equal? #f (check-range1 (create-temps 100 100 324)))
(equal? #f (check-range1 (cons 96 (create-temps 100 6 95))))

; We can simply feed the result of create-tmps into check-range1 provided
; that we choose appropriate values for lower and higher. For example,
; we can't feed the results of (create-temps 100 0 150) into check-range1
; since the return value of check-range1 will depend upon whether or not
; the 100 random numbers were all within the legal range. If, however
; we feed the results of (create-temps 100 6 95) or (create-temps 100 100 324)
; into check-range1 we're guaranteed to get true and false respectively.
; This tells us that we have to be careful when testing with automatically
; generated data since it can lead to flapping tests if we do not use
; sufficient constraints.


; --- 11.3.4 ---
; create-prices : number -> list-of-numbers
; Creates a list of n prices where each prices is between $.10 and $10.00
; and is evenly divisible by $.10
(define (create-prices n)
  (cond
    [(zero? n) empty]
    [else (cons (* n .10)
                (create-prices (sub1 n)))]))

(equal? (cons .20 (cons .10 empty))
        (create-prices 2))

(define (dollar-store? a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) true]
    [else (and (<= (first a-list-of-numbers) 1)
               (dollar-store? (rest a-list-of-numbers)))]))

(equal? #t (dollar-store? (create-prices 9)))
(equal? #f (dollar-store? (create-prices 11)))


; The following definitions and expressions are meant to be executed in
; DrRacket with the draw.rkt teachpack enabled.

; --- 11.3.5 ---
;(define ROWS 10)
;(define COLUMNS 10)
;(define ROW-HEIGHT 20)
;(define COL-WIDTH 20)
;(define CANVAS-HEIGHT (* ROW-HEIGHT ROWS))
;(define CANVAS-WIDTH (* COL-WIDTH COLUMNS))
;(define SPLAT-RADIUS 4)

; draw-grid : number number -> boolean
; draws a grid on the canvas with the specified number of rows and
; columns
;(define (draw-grid canvas-width canvas-height row-height col-width
;                   rows columns)
;  (and
;    (draw-rows canvas-width row-height rows)
;    (draw-cols canvas-height col-width columns)))

; draw-rows : number number -> boolean
; draws the specified number of rows where each row has the specified
; height
;(define (draw-rows canvas-width row-height n)
;  (cond
;    [(<= n 1) true]
;    [else (and
;            (draw-solid-line (make-posn 0 (* row-height (- n 1)))
;                             (make-posn canvas-width (* row-height (- n 1)))
;                             'black)
;            (draw-rows canvas-width row-height (sub1 n)))]))

; draw-cols : number number -> boolean
; draws the specified number of columns where each column has the
; specified height
;(define (draw-cols canvas-height col-width n)
;  (cond
;    [(<= n 1) true]
;    [else (and
;            (draw-solid-line (make-posn (* col-width (- n 1)) 0)
;                             (make-posn (* col-width (- n 1)) canvas-height)
;                             'black)
;            (draw-cols canvas-height col-width (sub1 n)))]))

; riot : number number -> boolean
; Draws the the simulated aftermath of a riot on the canvas where the
; specified number of balloons are thrown and each splat has the specified
; radius. Assumes splats are randomly distributed across the canvas.
;(define (riot canvas-width canvas-height splat-radius balloons)
;  (cond
;    [(zero? balloons) true]
;    [else (and
;            (draw-solid-disk
;              (make-posn (random-n-m 0 canvas-width)
;                         (random-n-m 0 canvas-height))
;              splat-radius
;              'red)
;            (riot canvas-width canvas-height splat-radius (sub1 balloons)))]))
;
;(start CANVAS-WIDTH CANVAS-HEIGHT)
;(draw-grid CANVAS-WIDTH CANVAS-HEIGHT ROW-HEIGHT COL-WIDTH ROWS COLUMNS)
;(riot CANVAS-WIDTH CANVAS-HEIGHT SPLAT-RADIUS 25)


; --- 11.4.1 ---
; ! : number -> number
; Computes the factorial of the passed in number
(define (! n)
  (cond
    [(zero? n) 1]
    [else (* n (! (sub1 n)))]))

(= 2 (! 2))
(= 6 (! 3))
(= 5040 (! 7))

; product : number number -> number
; Computes the product of natural numbers from n (exclusive) to
; m (inclusive). This is equal to (m! / n!). Assume n < m.
(define (product n m)
  (/ (! m) (! n)))

(= 30 (product 4 6))
(= 482718652416000 (product 7 20))


; --- 11.4.3 ---
; product-from-minus-11 : number [n >= -11] -> number
; Computes the product of integers between -11 (exclusive) and n
; (inclusive).
(define (product-from-minus-11 n)
  (cond
    [(= -11 n) 1]
    [else (* n (product-from-minus-11 (sub1 n)))]))

(= 1 (product-from-minus-11 -11))
(= -10 (product-from-minus-11 -10))
(= -720 (product-from-minus-11 -8))


; --- 11.4.4 ---
; tabulate-f20 : number [n >= 20] -> list-of-posns
; Constructs a list of posns where the posns are of the form
; [(n, f(n)) (n-1, f(n-1)) ... (21, f(21)]. The resulting list will contain
; n-20 elements.
(define (tabulate-f20 n)
  (cond
    [(= 20 n) empty]
    [else (cons (make-posn n (f n))
                (tabulate-f20 (sub1 n)))]))

(equal? (cons (make-posn 21 (f 21)) empty)
        (tabulate-f20 21))
(equal? (cons (make-posn 23 (f 23))
        (cons (make-posn 22 (f 22))
        (cons (make-posn 21 (f 21)) empty)))
        (tabulate-f20 23))


; --- 11.4.5 ---


; --- 11.4.6 ---


; --- 11.4.7 ---


; --- 11.5.1 ---


; --- 11.5.2 ---


; --- 11.5.3 ---


; --- 11.5.4 ---
