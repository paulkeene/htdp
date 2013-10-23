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
