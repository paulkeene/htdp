#lang racket

; --- 9.1.1 ---
; Planets in the solar system. Don't worry pluto. I got you.
(cons 'Mercury
  (cons 'Venus
    (cons 'Earth
      (cons 'Mars
        (cons 'Jupiter
          (cons 'Saturn
            (cons 'Uranus
              (cons 'Neptune
                (cons 'Pluto empty)))))))))
; Food items in meal: steak, pommes-frites, beans, bread, water, juice,
; brie-cheese, ice-cream
(cons 'Steak
  (cons 'Pommes-Frites
    (cons 'Beans
      (cons 'Bread
        (cons 'Water
          (cons 'Juice
            (cons 'Brie-Cheese
              (cons 'Ice-Cream empty))))))))
; Basic colors
(cons 'Red
  (cons 'Orange
    (cons 'Yellow
      (cons 'Green
        (cons 'Blue
          (cons 'Purple
            (cons 'Brown
              (cons 'Black empty))))))))


; --- 9.1.2 ---
; (cons 10 (cons 20 (cons 5 empty)))
;
; 1. (rest l) = (cons 20 (cons 5 empty))
; 2. (first (rest l)) = 20
; 3. (rest (rest l)) = (cons 5 empty)
; 4. (first (rest (rest l))) = 5
; 5. (rest (rest (rest l))) = empty


; --- 9.1.3 ---
; add-up-3 : list-of-3-numbers -> number
; to add up the three numbers in a-list-of-3-numbers
(define (add-up-3 a-list-of-3-numbers)
 (+ (first a-list-of-3-numbers)
    (first (rest a-list-of-3-numbers))
    (first (rest (rest a-list-of-3-numbers)))))

(define a-list (cons 42 (cons 22 (cons 170 empty))))
(define a-list-2 (cons 0 (cons 0 (cons 0 empty))))
(define a-list-3 (cons -1 (cons -90 (cons 35 empty))))

(= 234 (add-up-3 a-list))
(= 0 (add-up-3 a-list-2))
(= -56 (add-up-3 a-list-3))

(define (distance-to-0-for-3 a-list-of-3-numbers)
  (sqrt
    (+ (sqr (first a-list-of-3-numbers))
       (sqr (first (rest a-list-of-3-numbers)))
       (sqr (first (rest (rest a-list-of-3-numbers)))))))

(= 176.48795992928243 (distance-to-0-for-3 a-list))
(= 0 (distance-to-0-for-3 a-list-2))
(= 96.57121724406295 (distance-to-0-for-3 a-list-3))


; --- 9.1.4 ---
; A list of 2 symbols is a list:
;   (cons x (cons y empty))
; where x and y are symbols
;
; contains-2-doll? : list-f-2-symbols -> boolean
; to inspect a list of 2 symbols and return whether or not one of them
; is 'doll
(define (contains-2-doll a-list-of-2-symbols)
  (or (symbol=? (first a-list-of-2-symbols) 'doll)
      (symbol=? (first (rest a-list-of-2-symbols)) 'doll)))

(define a-symbol-list (cons 'apple (cons 'doll empty)))
(define a-symbol-list-2 (cons 'doll (cons 'orange empty)))
(define a-symbol-list-3 (cons 'grapefruit (cons 'cherry empty)))

(equal? #t (contains-2-doll a-symbol-list))
(equal? #t (contains-2-doll a-symbol-list-2))
(equal? #f (contains-2-doll a-symbol-list-3))


; --- 9.2.1 ---
; empty is a list-of-symbols because, by definition, a list-of-symbols
; is either empty or a symbol cons'd with a list-of-symbols.
;
; (cons 'ball empty) is a list of symbols. We already showed that
; empty is a list-of-symbols so cons'ing 'ball, which is a symbol
; with empty (a list of symbols) we, by definition have created
; a new list-of-symbols.
;
; (cons 'arrow (cons 'ball empty) is a list of symbols. We already showed
; that (cons 'ball empty) is a list of symbols. Cons'ing 'arrow, a symbol,
; with (cons 'ball empty), a list of symbols, results in a list of symbols
; by the second clause of our definition of list-of-symbols.
;
; (cons 'clown empty) is a list of symbols for the same reason that
; (cons 'ball empty) is a list of symbols. The actual symbols that make
; up the list are irrelevant.
;
; (cons 'bow (cons 'arrow (cons 'ball empty))) is a list of symbols. We
; already showd how a 2-element list, (cons 'arrow (cons 'ball empty)) is
; a list of symbols. This is the same thing with one more cons applied.
;
; (cons 'clown (cons 'bow (cons 'arrow (cons 'ball empty)))) is a list
; of symbosl due to the same reasoning as we applied for
; (cons 'bow (cons 'arrow (cons 'ball empty))).


; --- 9.2.2 ---
; All lists of two symbols belong to the class list-of-symbols since
; they all the following structure:
;   (cons x (cons y empty))
; where x and y are symbols. We know that empty is a list of symbols.
; Because of this (cons y empty) is also a list of symbols since y is
; a symbol and empty is a list of symbols. Building off of this fact
; (cons x (cons y empty)) is also a list of symbols since x is a symbol
; and (cons y empty) is a list of symbols.


; --- 9.2.3 ---
; A list of booleans is either
; 1. the empty list, empty, or
; 2. (cons b lob) where b is a boolean and lob is a list of booleans.


; --- 9.3.1 ---
(define (contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (cond
        [(symbol=? (first a-list-of-symbols) 'doll) true]
        [else (contains-doll? (rest a-list-of-symbols))])]))

(define a-symbol-list-4 (cons 'arrow (cons 'bow (cons 'doll
                          (cons 'water-gun empty)))))
(define a-symbol-list-5 empty)
(define a-symbol-list-6 (cons 'ball (cons 'car (cons 'stuffed-animal empty))))

(equal? #t (contains-doll? a-symbol-list-4))
(equal? #f (contains-doll? a-symbol-list-5))
(equal? #f (contains-doll? a-symbol-list-6))


; --- 9.3.2 ---
(define (alt-contains-doll? a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or
            (symbol=? (first a-list-of-symbols) 'doll)
            (alt-contains-doll? (rest a-list-of-symbols)))]))

(equal? #t (alt-contains-doll? a-symbol-list-4))
(equal? #f (alt-contains-doll? a-symbol-list-5))
(equal? #f (alt-contains-doll? a-symbol-list-6))


; --- 9.3.3 ---
(define (contains a-symbol a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) false]
    [else (or
            (symbol=? (first a-list-of-symbols) a-symbol)
            (alt-contains-doll? (rest a-list-of-symbols)))]))

(equal? #t (contains 'water-gun a-symbol-list-4))
(equal? #f (contains 'arrow a-symbol-list-5))
(equal? #f (contains 'board-game a-symbol-list-6))


; --- 9.5.1 ---
(define (sum a-list-of-nums)
  (cond
    [(empty? a-list-of-nums) 0]
    [else (+ (first a-list-of-nums) (sum (rest a-list-of-nums)))]))

(= 0 (sum empty))
(= 1.00 (sum (cons 1.00 empty)))
(= 20.86 (sum (cons 17.05 (cons 1.22 (cons 2.59 empty)))))
(= 2.59 (sum (cons 2.59 empty)))
(<= (abs (- 3.81 (sum (cons 1.22 (cons 2.59 empty))))) .001)


; --- 9.5.2 ---
; how-many-symbols : a-list-of-symbols -> number
; to return the number of elements in a list of symbols
(define (how-many-symbols a-list-of-symbols)
  (cond
    [(empty? a-list-of-symbols) 0]
    [else (+ 1 (how-many-symbols (rest a-list-of-symbols)))]))

(= 4 (how-many-symbols a-symbol-list-4))
(= 0 (how-many-symbols empty))


; how-many-numbers : a-list-of-numbers -> number
; to return the number of elements in a list of numbers
(define (how-many-numbers a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) 0]
    [else (+ 1 (how-many-numbers (rest a-list-of-numbers)))]))

(= 4 (how-many-symbols a-symbol-list-4))
(= 0 (how-many-symbols empty))

; these two functions are identical except for the names  I chose for
; their arguments


; dollar-store? : list-of-numbers -> boolean
; to determine if all of the goods in a store are priced less than $1.00

(define (dollar-store? a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) true]
    [else (and (<= (first a-list-of-numbers) 1)
               (dollar-store? (rest a-list-of-numbers)))]))

(define price-list (cons .99 (cons .50 (cons .33 (cons .25 (cons .01 empty))))))
(define price-list-2 (cons .35 (cons .68 (cons 1.99 (cons .25 empty)))))
(define price-list-3 (cons 5.00 (cons .23 empty)))

(equal? #t (dollar-store? price-list))
(equal? #f (dollar-store? price-list-2))
(equal? #f (dollar-store? price-list-3))
(equal? #t (dollar-store? empty))

(define (prices-under-threshold? threshold prices)
  (cond
    [(empty? prices) true]
    [else (and (<= (first prices) threshold)
               (prices-under-threshold? threshold (rest prices)))]))

(equal? #t (prices-under-threshold? 2.50 price-list-2))
(equal? #f (prices-under-threshold? 1.00 price-list-2))
(equal? #t (prices-under-threshold? 1.00 empty))


; --- 9.5.4 ---
; check-range1 : list-of-temperatures -> boolean
; to determine if all the temperatures in the passed-in list are between
; 5 and 95 degrees Celsius.

(define (check-range1 temperatures)
  (cond
    [(empty? temperatures) true]
    [else (and
      (and (< (first temperatures) 95) (> (first temperatures) 5))
      (check-range1 (rest temperatures)))]))

(define a-list-of-temps (cons 55.0 (cons 32.1 (cons 94.3 (cons 6.1 empty)))))
(define a-list-of-temps-2 (cons 92.1 (cons 86.3 (cons 97.2 (cons 92.5 empty)))))
(define a-list-of-temps-3 (cons 91 (cons 4.2 empty)))

(equal? #t (check-range1 empty))
(equal? #t (check-range1 a-list-of-temps))
(equal? #f (check-range1 a-list-of-temps-2))
(equal? #f (check-range1 a-list-of-temps-3))

(define (check-range lower-bound upper-bound temperatures)
  (cond
    [(empty? temperatures) true]
    [else (and
      (and (< (first temperatures) upper-bound)
           (> (first temperatures) lower-bound))
      (check-range lower-bound upper-bound (rest temperatures)))]))

(equal? #t (check-range 22 51 empty))
(equal? #t (check-range 5.9 98.6 a-list-of-temps))
(equal? #f (check-range -22.0 90.0 a-list-of-temps))
(equal? #f (check-range 7.0 97.6 a-list-of-temps))


; --- 9.5.5 ---
; convert : list-of-numbers -> number
; to convert a list of digits into the corresponding integer
(define (convert digits)
  (cond
    [(empty? digits) 'none]
    [(= (length digits) 1) (first digits)]
    [else (+ (first digits) (* 10 (convert (rest digits))))]))

(= 321 (convert (cons 1 (cons 2 (cons 3 empty)))))
(= 67 (convert (cons 7 (cons 6 empty))))
(= 0 (convert (cons 0 empty)))
(symbol=? 'none (convert empty))

; check-guess-for-list : list-of-numbers number -> symbol
; to return 'TooSmall, 'Perfect or 'TooLarge depending on whether the
; number represented by the list of digits is smaller, equal to, or
; larger than the hidden number.
(define (check-guess-for-list digits hidden-number)
  (cond
    [(< (convert digits) hidden-number) 'TooSmall]
    [(> (convert digits) hidden-number) 'TooLarge]
    [else 'Perfect]))


; --- 9.5.6 ---
; delta : list-of-numbers list-of-numbers -> number
; to compute the difference in value between the second list
; of prices and the first.
(define (delta initial-prices final-prices)
  (- (sum final-prices) (sum initial-prices)))

(= -6 (delta (cons 3 (cons 2 (cons 1 empty))) empty))
(= -6 (delta (cons 3 (cons 2 (cons 1 empty))) (cons 0 empty)))
(= 0 (delta (cons 3 (cons 2 (cons 1 empty)))
            (cons 3 (cons 2 (cons 1 empty)))))
(= 5 (delta (cons 1 empty) (cons 3 (cons 2 (cons 1 empty)))))


; --- 9.5.7 ---
; average-price : list-of-numbers -> number
; to compute the average price from the prices in the list
(define (average-price prices)
  (cond
    [(empty? prices) 0]
    [else (/ (sum prices) (how-many-numbers prices))]))

(= 2 (average-price (cons 3 (cons 2 (cons 1 empty)))))
(= 0 (average-price (cons 0 empty)))
(< (abs (- 19.85 (average-price (cons 31 (cons 1.05 (cons 27.5 empty)))))) .001)
(= 0 (average-price empty))


; --- 9.5.8 ---
; draw-circles : posn list-of-numbers -> boolean
; To draw a set of concentric red circles around the specified posn
; on a canvas. This function returns true if it succeeds in drawing
; all of the circles and false otherwise.
;
; The following code is meant to be executed inside DrRacket with the
; draw.rkt teachpack enabled.
;(define (draw-circles a-posn radii)
;  (cond
;    [(empty? radii) true]
;    [else (and
;            (draw-circle a-posn (first radii) 'red)
;            (draw-circles a-posn (rest radii)))]))
