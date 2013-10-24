#lang racket

; --- 10.1.1 ---
(define (hours->wages alon)
  (cond
    [(empty? alon) empty]
    [else (cons (wage (first alon)) (hours->wages (rest alon)))]))

(define (wage h)
  (* 12 h))

; If we want to give everyone a raise to $14 we simply modify wage to
; return (* 14 h) instead of (* 12 h)


; --- 10.1.2 ---
; If we want to prevent an employee from being paid for more than 100
; hours of work we can update the wage function to signal an error
; if the hours passed to it are greater than 100.


; --- 10.1.3 ---
; convertFC : list-of-numbers -> list-of-numbers
; Converts the temperatures from degrees Fahrenheit to degrees
; Celsius
(define (convertFC list-of-temps)
  (cond
    [(empty? list-of-temps) empty]
    [else (cons (to-celsius (first list-of-temps))
                (convertFC (rest list-of-temps)))]))

; to-celsius : number -> number
; Converts a temperature in degrees Fahrenheit to degrees
; Celsius
(define (to-celsius temperature)
  (* (- temperature 32) 5/9))

; to-fahrenheit : number -> number
; Converts a temperature in degrees Celsius to degrees
; Fahrenheit
(define (to-fahrenheit temperature)
  (+ (* temperature 9/5) 32))

(empty? (convertFC empty))
(= 0 (first (convertFC (cons 32 empty))))
(= -10 (first (convertFC (cons 14 (cons 86 empty)))))
(= 30 (first (rest (convertFC (cons 14 (cons 86 empty))))))


; --- 10.1.4 ---
; convert-euro : list-of-numbers -> list-of-numbers
; Converts a list of dollar amounts to a list of euro amounts
; where 1 dollar = 1.22 euros.
(define (convert-euro dollar-amounts)
  (cond
    [(empty? dollar-amounts) empty]
    [else (cons (* (first dollar-amounts) 1.22)
                (convert-euro (rest dollar-amounts)))]))

(empty? (convert-euro empty))
(= 1.22 (first (convert-euro (cons 1 empty))))
(= 51.24 (first (convert-euro (cons 42 (cons 58 empty)))))
(= 70.76 (first (rest (convert-euro (cons 42 (cons 58 empty))))))


(define (convert-euro-1 exchange-rate dollar-amounts)
  (cond
    [(empty? dollar-amounts) empty]
    [else (cons (* (first dollar-amounts) exchange-rate)
                (convert-euro-1 exchange-rate (rest dollar-amounts)))]))

(empty? (convert-euro-1 1.22 empty))
(= 1.22 (first (convert-euro-1 1.22 (cons 1 empty))))
(= 51.24 (first (convert-euro-1 1.22 (cons 42 (cons 58 empty)))))
(= 114.26 (first (rest (convert-euro-1 1.97 (cons 42 (cons 58 empty))))))


; --- 10.1.5 ---
; elimante-exp : number list-of-numbers -> list-of-numbers
; Returns the original list of prices after filtering all of the
; elements that are greater than the threshold
(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty]
    [else (cond
            [(<= (first lotp) ua) (cons (first lotp)
                                        (eliminate-exp ua (rest lotp)))]
            [else (eliminate-exp ua (rest lotp))])]))

(empty? (eliminate-exp 1.0 empty))
(equal? (cons .95 (cons 1.0 empty))
        (eliminate-exp 1.0 (cons 2.95 (cons .95 (cons 1.0 (cons 5 empty))))))


; --- 10.1.6 ---
; name-robot : list-of-symbols -> list-of-symbols
; Returns the original list of symbols with all occurences of 'robot
; substituted with 'r2d2
(define (name-robot names)
  (cond
    [(empty? names) empty]
    [else (cond
            [(symbol=? (first names) 'robot)
             (cons 'r2d2 (name-robot (rest names)))]
            [else (cons (first names) (name-robot (rest names)))])]))
(equal? (cons 'ball (cons 'r2d2 (cons 'doll (cons 'r2d2 empty))))
        (name-robot (cons 'ball (cons 'robot (cons 'doll
                    (cons 'robot empty))))))

; substitute : symbol symbol list-of-symbols -> list-of-symbols
; Returns the original list of symbols with all occurences of
; old substitued with new
(define (substitute new old toys)
  (cond
    [(empty? toys) empty]
    [else (cond
            [(symbol=? (first toys) old)
             (cons new (substitute new old (rest toys)))]
            [else (cons (first toys) (substitute new old (rest toys)))])]))

(equal? (cons 'ball (cons 'r2d2 (cons 'doll (cons 'r2d2 empty))))
        (substitute 'r2d2 'robot (cons 'ball (cons 'robot (cons 'doll
                                 (cons 'robot empty))))))
(equal? (cons 'robot (cons 'Barbie (cons 'dress empty)))
        (substitute 'Barbie 'doll (cons 'robot (cons 'doll
                                  (cons 'dress empty)))))


; --- 10.1.7 ---
; recall : symbol list-of-symbols -> list-of-symbols
; Filters out all of elements from the list that match the passed-in
; symbol
(define (recall ty toys)
  (cond
    [(empty? toys) empty]
    [else (cond
            [(symbol=? (first toys) ty) (recall ty (rest toys))]
            [else (cons (first toys) (recall ty (rest toys)))])]))

(equal? (cons 'doll (cons 'dress empty))
        (recall 'robot (cons 'robot (cons 'doll (cons 'dress empty)))))


; --- 10.1.8 ---
; quadratic-roots number number number -> list-of-numbers
; Compute the roots of the quadratic equation specified by the
; coefficients a, b, and c.
(define (quadratic-roots a b c)
  (cond
    [(= a 0) 'degenerate]
    [(< (sqr b) (* 4 a c)) 'none]
    [(= (sqr b) (* 4 a c)) (/ (- 0 b) (* 2 a))]
    [else (cons (/ (+ (- 0 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a))
          (cons (/ (- (- 0 b) (sqrt (- (sqr b) (* 4 a c)))) (* 2 a)) empty))]))

(equal? (cons 1 (cons -1 empty))
        (quadratic-roots 1 0 -1))
(= -1 (quadratic-roots 2 4 2))
(equal? (cons 0 (cons -5/3 empty))
        (quadratic-roots 3 5 0))
(symbol=? 'none (quadratic-roots 4 0 2))
(symbol=? 'degenerate (quadratic-roots 0 3 6))


; --- 10.1.9 ---
; controller : number -> list-of-numbers-and-symbols
; Computes the response that the cash machine should use based on the
; number of cents it is returning to the customer. The general template
; is as follows:
; [x, 'dollar(s), 'and, y, 'cent(s)]
; where x and y are numbers. The second element will be 'dollar if x=1
; and otherwise will be 'dollars. Similarly, the last element will be
; 'cent if y=1 and 'cents otherwise.
(define (controller total-cents)
  (cons (dollars total-cents)
    (cons (cond
            [(= (dollars total-cents) 1) 'dollar]
            [else 'dollars])
      (cons 'and
        (cons (left-over-cents total-cents)
          (cons (cond
                  [(= (left-over-cents total-cents) 1) 'cent]
                  [else 'cents]) empty))))))

(define (dollars total-cents)
  (quotient total-cents 100))

(define (left-over-cents total-cents)
  (remainder total-cents 100))

(equal? (cons 1 (cons 'dollar (cons 'and (cons 3 (cons 'cents empty)))))
        (controller 103))
(equal? (cons 1 (cons 'dollar (cons 'and (cons 1 (cons 'cent empty)))))
        (controller 101))
(equal? (cons 29 (cons 'dollars (cons 'and (cons 67 (cons 'cents empty)))))
        (controller 2967))

; checked-controller : number -> list-of-numbers-and-symbols
; Checks the input to see if the dollar and cent amounts are in the
; set of values that the teachpack has sounds for. If so this
; function returns the normal controller output. Otherwise it returns
; the empty list.
(define (checked-controller total-cents)
  (cond
    [(and (value-has-sound? (dollars total-cents))
          (value-has-sound? (left-over-cents total-cents)))
     (controller total-cents)]
    [else empty]))

(define (value-has-sound? value)
  (or (= value 0) (= value 20) (= value 30)
      (= value 40) (= value 50) (= value 60)
      (= value 70) (= value 80) (= value 90)))

(equal? empty (checked-controller 103))
(equal? (cons 40 (cons 'dollars (cons 'and (cons 20 (cons 'cents empty)))))
        (controller 4020))


; --- 10.2.1 ---
(define-struct ir (name price))

; contains-doll? : inventory -> boolean
; to determine whether an-inv contains a record for 'doll
(define (contains-doll? an-inv)
  (cond
    [(empty? an-inv) false]
    [else (or (symbol=? (ir-name (first an-inv)) 'doll)
              (contains-doll? (rest an-inv)))]))

(equal? #t (contains-doll? (cons (make-ir 'ball 1.0) (cons (make-ir 'doll 2.0)
                           (cons (make-ir 'truck 1.5) empty)))))
(equal? #f (contains-doll? (cons (make-ir 'ball 1.0)
                           (cons (make-ir 'truck 1.5) empty))))

; contains? : symbol inventory -> boolean
; to determine whether inventory contains a record for a-symbol
(define (contains? a-symbol inventory)
  (cond
    [(empty? inventory) false]
    [else (or (symbol=? (ir-name (first inventory)) a-symbol)
              (contains? a-symbol (rest inventory)))]))

(equal? #t (contains? 'truck (cons (make-ir 'ball 1.0)
                             (cons (make-ir 'doll 2.0)
                             (cons (make-ir 'truck 1.5) empty)))))
(equal? #f (contains? 'bicycle (cons (make-ir 'ball 1.0)
                               (cons (make-ir 'doll 2.0)
                               (cons (make-ir 'truck 1.5) empty)))))


; --- 10.2.2 ---
; An inventory record (2) is a structure:
;   (make-ir-2 name price image)
; where name is a symbol, price is a number, and image is an image.
;
; An inventory is either:
; 1. empty or
; 2. (cons ir-2 inv)
;    where ir2 is an inventory record (2) and inv is an inventory
;
; The inventory from Figure 29 can be represented as follow
; (cons (make-ir-2 'robot 11.95 'image1)
;   (cons (make-ir-2 'doll 19.95 'image2)
;     (cons (make-ir-2 'rocket 29.95 'image3) empty)))

(define-struct ir-2 (name price image))

; show-picture : symbol inventory -> image
; Returns the image of the toy with the specified name or false
; if there is no toy in the inventory with the specified name.
(define (show-picture name inventory)
  (cond
    [(empty? inventory) false]
    [else (cond
            [(symbol=? (ir-2-name (first inventory)) name)
             (ir-2-image (first inventory))]
            [else (show-picture name (rest inventory))])]))

(symbol=? 'image2
          (show-picture 'doll (cons (make-ir-2 'robot 11.95 'image1)
                              (cons (make-ir-2 'doll 19.95 'image2)
                              (cons (make-ir-2 'rocket 29.95 'image3) empty)))))
(equal? #f
        (show-picture 'ball (cons (make-ir-2 'robot 11.95 'image1)
                            (cons (make-ir-2 'doll 19.95 'image2)
                            (cons (make-ir-2 'rocket 29.95 'image3) empty)))))


; --- 10.2.3
; price-of : symbol inventory -> number
; Returns the price of the toy with the specified name or false
; if there is no toy in the inventory with the specified name.
(define (price-of name inventory)
  (cond
    [(empty? inventory) false]
    [else (cond
            [(symbol=? (ir-2-name (first inventory)) name)
             (ir-2-price (first inventory))]
            [else (price-of name (rest inventory))])]))

(= 19.95
   (price-of 'doll (cons (make-ir-2 'robot 11.95 'image1)
                   (cons (make-ir-2 'doll 19.95 'image2)
                   (cons (make-ir-2 'rocket 29.95 'image3) empty)))))
(equal? #f
   (price-of 'ball (cons (make-ir-2 'robot 11.95 'image1)
                   (cons (make-ir-2 'doll 19.95 'image2)
                   (cons (make-ir-2 'rocket 29.95 'image3) empty)))))


; A phone record is a structure:
;   (make-phone-record name number)
; where name and number are symbols.
;
; A directoy is either:
; 1. empty or
; 2. (cons pr dir)
;    where pr is a phone record and dir is a directory.

(define-struct phone-record (name number))

; whose-number : symbol directory -> symbol
; Returns the name associated with the given phone number
(define (whose-number number directory)
  (cond
    [(empty? directory) false]
    [else (cond
            [(symbol=? (phone-record-number (first directory)) number)
             (phone-record-name (first directory))]
            [else (whose-number number (rest directory))])]))

(symbol=? 'Jenny
          (whose-number '619-208-7176
                        (cons (make-phone-record 'David '415-206-3232)
                        (cons (make-phone-record 'Chance '760-324-8275)
                        (cons (make-phone-record 'Jenny '619-208-7176)
                        empty)))))
(equal? #f
        (whose-number '555-213-8584
                      (cons (make-phone-record 'David '415-206-3232)
                      (cons (make-phone-record 'Chance '760-324-8275)
                      (cons (make-phone-record 'Jenny '619-208-7176)
                      empty)))))

; phone-number : symbol directory -> symbol
; Returns the phone number associated with the given name
(define (phone-number name directory)
  (cond
    [(empty? directory) false]
    [else (cond
            [(symbol=? (phone-record-name (first directory)) name)
             (phone-record-number (first directory))]
            [else (phone-number name (rest directory))])]))

(symbol=? '760-324-8275
          (phone-number 'Chance
                        (cons (make-phone-record 'David '415-206-3232)
                        (cons (make-phone-record 'Chance '760-324-8275)
                        (cons (make-phone-record 'Jenny '619-208-7176)
                        empty)))))
(equal? #f
        (phone-number 'Rick
                      (cons (make-phone-record 'David '415-206-3232)
                      (cons (make-phone-record 'Chance '760-324-8275)
                      (cons (make-phone-record 'Jenny '619-208-7176)
                      empty)))))


; --- 10.2.5 ---
; extract>1 : inventory -> inventory
; Creates an inventory from the initial inventory for all items that
; cost more than 1 dollar.
(define (extract>1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (cond
          [(> (ir-price (first an-inv)) 1)
           (cons (first an-inv) (extract>1 (rest an-inv)))]
          [else (extract>1 (rest an-inv))])]))

(symbol=? 'robot
          (ir-name (first (extract>1 (cons (make-ir 'robot 1.95)
                                     (cons (make-ir 'doll 0.95)
                                     (cons (make-ir 'rocket 2.95) empty)))))))
(symbol=? 'rocket
          (ir-name (first (rest
          (extract>1 (cons (make-ir 'robot 1.95)
                     (cons (make-ir 'doll 0.95)
                     (cons (make-ir 'rocket 2.95) empty))))))))


; --- 10.2.6 ---
; An inventory1 is either:
; 1. empty or
; 2. (cons ir inv)
;    where ir is an inventory record with a price less than or equal to
;    $1 and inv is an inventory1.
;
; extract1 : inventory -> inventory1
; (define (extract1 an-inv) ...)
;
; The refined contract does not affect the development of the above
; function compared to the previous definition of extract1. If we
; wanted we could define a new flavor of ir which checkes upon
; construction that the price of the toy is less than or equal
; to $1 and throws an error on invalid inputs. If we did this then
; we would need to use that alternate ir inside this version of
; extract1.


; --- 10.2.7 ---
; raise-prices : inventory -> inventory
; Creates an inventory based on the original where each price is
; 5% greater than the corresponding price in the original inventory.
(define (raise-prices an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (cons (make-ir
                  (ir-name (first an-inv))
                  (* (ir-price (first an-inv)) 1.05))
                (raise-prices (rest an-inv)))]))

(= 2.0475
   (ir-price (first (raise-prices
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))
(> .001 (- .9975
   (ir-price (first (rest (raise-prices
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))))

(= 3.0975
   (ir-price (first (rest (rest (raise-prices
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))))


; --- 10.2.8 ---
; recall2 : ty inventory -> inventory
; Creates an inventory from the original where all toys with the name
; ty have been filtered out.
(define (recall2 ty an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (cond
            [(symbol=? (ir-name (first an-inv)) ty)
             (recall2 ty (rest an-inv))]
            [else (cons (first an-inv) (recall2 ty (rest an-inv)))])]))

(symbol=? 'robot
   (ir-name (first (recall2 'doll
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))
(symbol=? 'rocket
   (ir-name (first (rest (recall2 'doll
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty))))))))


; --- 10.2.9 ---
; name-robot : inventory -> inventory
; Creates an inventory from the original where all toys with the name
; 'robot are changed to have the name 'r2d3.
(define (name-robot2 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (cond
            [(symbol=? (ir-name (first an-inv)) 'robot)
             (cons
               (make-ir 'r2d3 (ir-price (first an-inv)))
               (name-robot2 (rest an-inv)))]
            [else
              (cons (first an-inv) (name-robot2 (rest an-inv)))])]))

(symbol=? 'r2d3
   (ir-name (first (name-robot2
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))
(symbol=? 'doll
   (ir-name (first (rest (name-robot2
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty))))))))
(symbol=? 'rocket
   (ir-name (first (rest (rest (name-robot2
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))))

; substitute2 : symbol symbol inventory -> inventory
; Creates an inventory from the original where all toys with the name
; old are changed to have the name new.
(define (substitute2 new old an-inv)
  (cond
    [(empty? an-inv) empty]
    [else (cond
            [(symbol=? (ir-name (first an-inv)) old)
             (cons
               (make-ir new (ir-price (first an-inv)))
               (substitute2 new old (rest an-inv)))]
            [else
              (cons (first an-inv) (substitute2 new old (rest an-inv)))])]))

(symbol=? 'robot
   (ir-name (first (substitute2 'barbie 'doll
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))
(symbol=? 'barbie
   (ir-name (first (rest (substitute2 'barbie 'doll
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty))))))))
(symbol=? 'rocket
   (ir-name (first (rest (rest (substitute2 'barbie 'doll
                      (cons (make-ir 'robot 1.95)
                      (cons (make-ir 'doll 0.95)
                      (cons (make-ir 'rocket 2.95) empty)))))))))


; --- The following functions are meant to be executed in DrRacket with
;     the draw.rkt teachpack enabled ---


; --- 10.3.1 ---
; A list of shapes is either:
; 1. empty or
; 2. (cons s los)
;    where s is a shape and los is a list of shapes.
;
; A shape is either:
; 1. A rectangle:
;      (make-rectangle p w l c)
;    where p is a posn w is a number l is a number and c is a symbol.
; 2. A circle:
;      (make-circle p r c)
;    where p is a posn r is a number and c is a symbol.


;(define-struct rectangle (nw width height color))
;(define-struct circle (center radius color))

;(define FACE
;  (cons (make-circle (make-posn 50 50) 40 'red)
;  (cons (make-rectangle (make-posn 30 20) 5 5 'blue)
;  (cons (make-rectangle (make-posn 65 20) 5 5 'blue)
;  (cons (make-rectangle (make-posn 40 75) 20 10 'red)
;  (cons (make-rectangle (make-posn 45 35) 10 30 'blue)
;  empty))))))

; fun-for-losh : list-of-shapes -> ???
; (define (fun-for-losh a-losh)
;    (cond
;      [(empty? a-lost) a-losh ...]
;      [else ... (first a-losh) ... (fun-for-losh (rest a-losh)) ...]))


; --- 10.3.2 ---
; draw-losh : list-of-shapes -> boolean
; Draws each shape in the list of shapes on the canvas
;(define (draw-losh a-losh)
;  (cond
;    [(empty? a-losh) true]
;    [else (and
;            (draw-shape (first a-losh))
;            (draw-losh (rest a-losh)))]))

;(define (draw-a-rectangle a-rectangle)
;  (draw-solid-rect
;    (rectangle-nw a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)
;    (rectangle-color a-rectangle)))

;(define (draw-a-circle a-circle)
;  (draw-circle
;    (circle-center a-circle)
;    (circle-radius a-circle)
;    (circle-color a-circle)))

;(define (draw-shape a-shape)
;  (cond
;    [(rectangle? a-shape) (draw-a-rectangle a-shape)]
;    [(circle? a-shape) (draw-a-circle a-shape)]
;    [else false]))


; --- 10.3.3 ---
; translate-losh : list-of-shapes number -> list-of-shapes
; Moves each shape in the list of shapes delta units on the x-axis
;(define (translate-losh a-losh delta)
;  (cond
;    [(empty? a-losh) empty]
;    [else (cons (cond
;            [(circle? (first a-losh))
;             (make-circle
;               (make-posn (+ (posn-x (circle-center (first a-losh))) delta)
;                          (posn-y (circle-center (first a-losh))))
;               (circle-radius (first a-losh))
;               (circle-color (first a-losh)))]
;            [else
;             (make-rectangle
;               (make-posn (+ (posn-x (rectangle-nw (first a-losh))) delta)
;                          (posn-y (rectangle-nw (first a-losh))))
;               (rectangle-width (first a-losh))
;               (rectangle-height (first a-losh))
;               (rectangle-color (first a-losh)))])
;            (translate-losh (rest a-losh) delta))]))


; --- 10.3.4 ---
; clear-losh : list-of-shapes -> boolean
; Clears each shape in the list of shapes on the canvas
;(define (clear-losh a-losh)
;  (cond
;    [(empty? a-losh) true]
;    [else (and
;            (clear-shape (first a-losh))
;            (clear-losh (rest a-losh)))]))

;(define (clear-a-rectangle a-rectangle)
;  (clear-solid-rect
;    (rectangle-nw a-rectangle)
;    (rectangle-width a-rectangle)
;    (rectangle-height a-rectangle)
;    (rectangle-color a-rectangle)))

;(define (clear-a-circle a-circle)
;  (clear-circle
;    (circle-center a-circle)
;    (circle-radius a-circle)
;    (circle-color a-circle)))

;(define (clear-shape a-shape)
;  (cond
;    [(rectangle? a-shape) (clear-a-rectangle a-shape)]
;    [(circle? a-shape) (clear-a-circle a-shape)]
;    [else false]))

; --- 10.3.5 ---
; draw-and-clear-picture : list-of-shapes number -> boolean
; Draws a list of shapes to the canvas, sleeps for the specified number
; of seconds, and clears the same shapes from the canvas.
;(define (draw-and-clear-picture a-losh seconds)
;  (and
;    (draw-losh a-losh)
;    (sleep-for-a-while seconds)
;    (clear-losh a-losh)))


; --- 10.3.6 ---
; move-picture : list-of-shapes delta -> list-of-shapes
; Draws each shape in the list of shapes, sleeps for a duration,
; clears each shape in the list, and then returns the list of shapes
; translated by delta along the x-axis.
;(define (move-picture delta a-losh)
;  (cond
;    [(draw-and-clear-picture a-losh 1)
;     (translate-losh a-losh delta)]
;    [else a-losh]))
