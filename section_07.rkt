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

(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* (square-length a-shape) 4)]
    [(circle? a-shape) (* 2 pi (circle-radius a-shape))]))

(define my-square (make-square (make-posn 0 0) 5))
(define my-circle (make-circle (make-posn 0 0) 3))

(= (perimeter my-square) 20)
(= (perimeter my-circle) (* 2 pi 3))


; --- 7.1.3 ---
(define (area a-shape)
  (cond
    [(square? a-shape) (sqr (square-length a-shape))]
    [(circle? a-shape) (* pi (sqr (circle-radius a-shape)))]))

(= (area my-square) 25)
(= (area my-circle) (* pi 9))


; --- 7.2.1 ---
; A spider is a structure:
;   (make-spider legs space)
; where legs is a number (integer) and space is a number (float)
;
; An elephant is a structure:
;   (make-elephant space)
; where space is a number (float)
;
; A monkey is a structure:
;   (make-monkey intelligence space)
; where intelligence is a symbol and space is a number (float)
;
; f : animal -> ???
; (define (f animal)
;   (cond
;       [(spider? animal) ... (spider-legs animal) ... (spider-size animal)]
;       [(elephant? animal) ... (elephant-size animal)]
;       [(monkey? animal) ... (monkey-intelligence animal)
;                         ... (monkey-size animal)]))

(define-struct spider (legs size))
(define-struct elephant (size))
(define-struct monkey (intelligence size))

(define (fits? animal volume)
  (cond
    [(spider? animal)
     (<= (spider-size animal) volume)]
    [(elephant animal)
     (<= (elephant-size animal) volume)]
    [(monkey? animal)
     (<= (monkey-size animal) volume)]))

(define my-spider (make-spider 8 2.0))
(define my-elephant (make-elephant 100.0))

(equal? (fits? my-spider 5.0) #t)
(equal? (fits? my-elephant 75.0) #f)


; --- 7.2.2 ---
; A bus is a structure:
;   (make-bus license-plate passengers)
; where license-plate is a symbol and passengers is a number
;
; A limo is a structure:
;   (make-limo license-plate model passengers)
; where license-plate and model are symbols and passengers is a number
;
; A car is a structure:
;   (make-car license-plate model color passengers)
; where license-place, model, and color are symbols and passengers is a number
;
; A subway is a structure:
;   (make-subway route-name passengers)
; where route-name is a symbol and passengers is a number
;
; f : vehicle -> ???
; (define (f vehicle)
;   (cond
;     [(bus? vehicle) ...
;      (bus-license-plate vehicle) ...
;      (bus-passengers vehicle) ... ]
;     [(limo? vehicle) ...
;      (limo-license-plate vehicle) ...
;      (limo-model vehicle) ...
;      (limo-passengers vehicle) ... ]
;     [(car? vehicle) ...
;      (car-license-plate vehicle) ...
;      (car-color vehicle) ...
;      (car-model vehicle) ...
;      (car-passengers vehicle) ... ]
;     [(subway? vehicle) ...
;      (subway-route-name vehicle) ...
;      (subway-passengers vehicle) ... ]))


; --- 7.3.1 ---
(define-struct rectangle (nw width height))

(define (perimeter-2 a-shape)
  (cond
    [(square? a-shape) (perimeter-square a-shape)]
    [(circle? a-shape) (perimeter-circle a-shape)]
    [(rectangle? a-shape) (perimeter-rectangle a-shape)]))

(define (perimeter-square a-square)
  (* (square-length a-square) 4))

(define (perimeter-circle a-circle)
    (* 2 pi (circle-radius a-circle)))

(define (perimeter-rectangle a-rectangle)
  (+ (* (rectangle-width a-rectangle) 2)
     (* (rectangle-height a-rectangle) 2)))

(define my-square-2 (make-square (make-posn 0 0) 6))
(define my-circle-2 (make-circle (make-posn 0 0) 5))
(define my-rectangle (make-rectangle (make-posn 0 0) 5 3))

(= (perimeter-2 my-square-2) 24)
(= (perimeter-2 my-circle-2) (* 2 pi 5))
(= (perimeter-2 my-rectangle) 16)


; --- 7.4.1 ---
; A shape is either
; 1. A strucutre: (make-rectange nw width height)
;    where nw is a posn and width and height are numbers
; 2. A structure (make-circle center radius)
;    where center is a posn and radius is a number
; 3. A triangle (make-triangle a b c)
;    where a, b, and c are posns
;
; fun-for-shape : shape -> ???
; (define (fun-for-shape a-shape)
;   (cond
;     [(rectangle?
;       (rectangle-nw a-shape) ...
;       (rectangle-width a-shape) ...
;       (rectangle-height a-shape) ... )]
;     [(circle?
;       (circle-center a-shape) ...
;       (circle-radius a-shape) ... )]
;     [(triangle?
;       (triangle-a a-shape) ...
;       (triangle-b a-shape) ...
;       (triangle-c a-shape) ... )]))

(define-struct triangle (a b c))


; --- 7.4.2 ---
; The following functions will are meant to be executed in DrRacket
; with the draw.rkt teachpack enabled

;(define (draw-a-rectangle a-rectangle)
;  (draw-solid-rect
;    (rectangle-nw a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)
;    'green))
;
;(define (draw-a-circle a-circle)
;  (draw-circle
;    (circle-center a-circle)
;    (circle-radius a-circle)
;    'red))
;
;(define (draw-a-triangle a-triangle)
;  (and
;    (draw-solid-line (triangle-a a-triangle) (triangle-b a-triangle) 'blue)
;    (draw-solid-line (triangle-b a-triangle) (triangle-c a-triangle) 'blue)
;    (draw-solid-line (triangle-c a-triangle) (triangle-a a-triangle) 'blue)))
;
;(define (draw-shape a-shape)
;  (cond
;    [(rectangle? a-shape) (draw-a-rectangle a-shape)]
;    [(circle? a-shape) (draw-a-circle a-shape)]
;    [(triangle? a-shape) (draw-a-triangle a-shape)]))


; --- 7.4.3 ---
(define (translate-rectangle a-rectangle delta)
  (make-rectangle
    (make-posn (+ (posn-x (rectangle-nw a-rectangle)) delta)
               (posn-y (rectangle-nw a-rectangle)))
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)))

(define (translate-circle a-circle delta)
  (make-circle
    (make-posn (+ (posn-x (circle-center a-circle)) delta)
               (posn-y (circle-center a-circle)))
    (circle-radius a-circle)))

(define (translate-triangle a-triangle delta)
  (make-triangle
    (make-posn (+ (posn-x (triangle-a a-triangle)) delta)
               (posn-y (triangle-a a-triangle)))
    (make-posn (+ (posn-x (triangle-b a-triangle)) delta)
               (posn-y (triangle-b a-triangle)))
    (make-posn (+ (posn-x (triangle-c a-triangle)) delta)
               (posn-y (triangle-c a-triangle)))))

(define (translate-shape a-shape delta)
  (cond
    [(rectangle? a-shape) (translate-rectangle a-shape delta)]
    [(circle? a-shape) (translate-circle a-shape delta)]
    [(triangle? a-shape) (translate-triangle a-shape delta)]))


; --- 7.4.4 ---
; The following functions will are meant to be executed in DrRacket
; with the draw.rkt teachpack enabled
;
;(define (clear-a-rectangle a-rectangle)
;  (clear-solid-rect
;    (rectangle-nw a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)
;    'green))
;
;(define (clear-a-circle a-circle)
;  (clear-circle
;    (circle-center a-circle)
;    (circle-radius a-circle)
;    'red))
;
;(define (clear-a-triangle a-triangle)
;  (and
;    (clear-solid-line (triangle-a a-triangle) (triangle-b a-triangle) 'blue)
;    (clear-solid-line (triangle-b a-triangle) (triangle-c a-triangle) 'blue)
;    (clear-solid-line (triangle-c a-triangle) (triangle-a a-triangle) 'blue)))
;
;(define (clear-shape a-shape)
;  (cond
;    [(rectangle? a-shape) (clear-a-rectangle a-shape)]
;    [(circle? a-shape) (clear-a-circle a-shape)]
;    [(triangle? a-shape) (clear-a-triangle a-shape)]))


; --- 7.4.5 ---
; The following functions will are meant to be executed in DrRacket
; with the draw.rkt teachpack enabled
;
;(define (draw-and-clear-shape a-shape seconds)
;  (and
;    (draw-shape a-shape)
;    (sleep-for-a-while seconds)
;    (clear-shape a-shape)))
;
;
; --- 7.4.6 ---
;(define (move-shape a-shape delta)
;  (cond
;    [(draw-and-clear-shape a-shape 1)
;     (translate-shape a-shape delta)]
;    [else a-shape]))


; --- 7.5.1 ---
(define (checked-area-of-disk r)
  (cond
    [(and (number? r) (> r 0)) (* pi (sqr r))]
    [else (error 'checked-area-of-disk "positive numerical radius expected")]))


; --- 7.5.2 ---
(define (attendees ticket-price)
  (+ 120
     (* 15
         (/ (- 5.0 ticket-price) 0.1))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(define (revenue ticket-price)
  (* ticket-price
     (attendees ticket-price)))

(define (cost ticket-price)
  (+ 180.0
     (* 0.04 (attendees ticket-price))))

(define (checked-profit ticket-price)
  (cond
    [(and (number? ticket-price) (> ticket-price 0)) (profit ticket-price)]
    [else
     (error 'checked-profit "positive numerical expected for ticket-price")]))

(define (checked-between-5-6 n)
  (cond
    [(number? n) (and (< 5 n) (< n 6))]
    [else
     (error 'checked-between-5-6 "positive numerical expected for n")]))

(define (checked-reply s)
  (cond
    [(not (symbol? s))
     (error 'checked-reply "symbol expected for s")]
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

(define (checked-distance-to-0 a-posn)
  (cond
    [(not (posn? a-posn))
     (error 'checked-distance-to-0 "expected a-posn to be of type posn")]
    [else
      (sqrt
        (+ (sqr (posn-x a-posn))
           (sqr (posn-y a-posn))))]))

(define (checked-perimeter a-shape)
  (cond
    [(or (circle? a-shape)
         (square? a-shape))
     (perimeter a-shape)]
    [else
     (error 'checked-perimeter
            "expected a-shape to be of type circle or square")]))


; --- 7.5.3 ---
(define-struct vec (x y))

(define (checked-make-vec x y)
  (cond
    [(and (> x 0) (> y 0))
     (make-vec x y)]
    [else
     (error 'checked-make-vec
            "expected x and y to be positive numbers")]))
