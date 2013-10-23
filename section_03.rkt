#lang racket

; --- 3.1.1, 3.1.2 ---
; Examples:
; Ticket price = $3:
; When the ticket prices is $3 we will have 120 attendees plus
; 15 * ($5 - $3) / $0.10 = 300 for a total of 420 attendees. So the
; total revenue is $3.00 * 420 = $1260. The cost will be $180 plus
; $0.04 * 420 = $16.80 for a total cost of $196.80. The profit is
; simply total revenue minus total cost which is
; $1260 - $196.80 = $1063.20
;
; Ticket price = $4
; Attendees = 120 + (15 * ((5 - 4) / .10) = 270
; Revenue = 270 * $4 = $1080
; Cost = $180 + ($0.04 * 270) = $190.80
; Profit = $1080 - $190.80 = $889.20
;
; Ticket price = $5
; Attendees = 120 + ((5 - 5) / .10) = 120
; Revenue = 120 * $5 = $600
; Cost = $180 + ($0.04 * 120) = $184.80
; Profit = $600 - $184.80 = $415.20
;
; General Rule
;
; Attendees = 120 + (15 * ((5.00 - ticket-price) / 0.1))
; Revenue = ticket-price * attendees
; Cost = 180 + (0.04 * attendees)
; Profit = revenue - cost
;
; The best price for maximizing profit is $5.00

(define (attendees ticket-price)
  (+ 120
     (* 15
         (/ (- 5.0 ticket-price) 0.1))))

"Tests for attendees"

420.0
(attendees 3.0)

270.0
(attendees 4.0)

120.0
(attendees 5.0)

(define (revenue ticket-price)
  (* ticket-price
     (attendees ticket-price)))

"Tests for ticket-price"

1260.0
(revenue 3.0)

1080.0
(revenue 4.0)

600.0
(revenue 5.0)

(define (cost ticket-price)
  (+ 180.0
     (* 0.04 (attendees ticket-price))))

"Tests for cost"

196.8
(cost 3.0)

190.8
(cost 4.0)

184.8
(cost 5.0)

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

"Tests for profit"

1063.2
(profit 3.0)

889.2
(profit 4.0)

415.2
(profit 5.0)


; --- 3.1.3 ---

(define (bad-profit price)
  (- (* (+ 120
           (* (/ 15 .10)
              (- 5.00 price)))
        price)
     (+ 180
        (* .04
           (+ 120
              (* (/ 15 .10)
                 (- 5.00 price)))))))

"Tests for bad-profit"

1063.2
(bad-profit 3.0)

889.2
(bad-profit 4.0)

415.2
(bad-profit 5.0)


; --- 3.1.4 ---
(define (cost2 ticket-price)
 (* 1.5 (attendees ticket-price)))

(define (profit2 ticket-price)
  (- (revenue ticket-price)
     (cost2 ticket-price)))

"Profits with new cost structure"

"$3"
(profit2 3.0)

"$4"
(profit2 4.0)

"$5"
(profit2 5.0)


; --- 3.2.1 ---
(define base-attendees 120)
(define base-price 5.0)
(define attendence-boost-amount 15)
(define price-threshold 0.10)

(define (attendees3 ticket-price)
  (+ base-attendees
     (* attendence-boost-amount
         (/ (- base-price ticket-price) price-threshold))))

(define (revenue3 ticket-price)
  (* ticket-price
     (attendees3 ticket-price)))

(define base-cost 180.0)
(define cost-per-attendee 0.04)
(define (cost3 ticket-price)
  (+ base-cost
     (* cost-per-attendee (attendees3 ticket-price))))

(define (profit3 ticket-price)
  (- (revenue3 ticket-price)
     (cost3 ticket-price)))

"Tests for re-worked profit"

1063.2
(profit3 3.0)

889.2
(profit3 4.0)

415.2
(profit3 5.0)


; --- 3.3.1 ---
(define cm-per-inch 2.54)
(define inches-per-foot 12)
(define feet-per-yard 3)
(define yards-per-rod 5.5)
(define rods-per-furlong 40)
(define furlongs-per-mile 8)

(define (inches->cm inches)
  (* cm-per-inch inches))

(define (feet->inches feet)
  (* inches-per-foot feet))

(define (yards->feet yards)
  (* feet-per-yard yards))

(define (rods->yards rods)
  (* yards-per-rod rods))

(define (furlongs->rods furlongs)
  (* rods-per-furlong furlongs))

(define (miles->furlongs miles)
  (* furlongs-per-mile miles))

(define (feet->cm feet)
  (inches->cm (feet->inches feet)))

(define (yards->cm yards)
  (feet->cm (yards->feet yards)))

(define (rods->inches rods)
  (feet->inches (yards->feet (rods->yards rods))))

(define (miles->feet miles)
  (yards->feet (rods->yards (furlongs->rods (miles->furlongs miles)))))


; --- 3.3.2 ---
(define PI 3.14159)

; Example:
; V = pi * (r ^ 2) * h
; Let r = 3, h = 7
; V = pi * (3 ^ 2) * 7 = 197.92017
(define (volume-cylinder r h)
  (* PI
     (* (* r r)
        h)))

"Tests for volume-cylinder"

197.92017
(volume-cylinder 3 7)


; --- 3.3.3 ---
; Example:
; A = 2 * pi * (r ^ 2) + 2 * pi * r * h
; Let r = 3, h = 7
; A = 2 * pi * (3 ^ 2) + 2 * pi * 3 * 7 = 188.4954
(define (area-cylinder r h)
  (+
      (* 2
         (* PI
            (* r r)))
      (* 2
         (* PI
            (* r h)))))

"Tests for area-cylinder"

188.4954
(area-cylinder 3 7)


; --- 3.3.4 ---
; Example:
; The area of a pipe is just the area of the cylinder with radius r
; minus the the area of the opening on each end (2 * pi * (r - t) ^ 2)
; Let r = 3, h = 7, t = 1
; area-cylinder = 188.4954
; area-opening = pi * (r - t) ^ 2 = 12.56636
; area-pipe = area-cylinder - (2 * area-opening) = 163.36268
(define (area-pipe r h t)
  (- (area-cylinder r h)
     (* 2
        (* PI
           (expt
             (- r t)
             2)))))

"Tests for area-pipe"

163.36268
(area-pipe 3 7 1)


; --- 3.3.5 ---
(define (height g t)
  (* t
      (* .5
         (* g t))))


; --- 3.3.6 ---
(define (Celsius->Fahrenheit degrees-celsius)
  (+ 32
     (/ (* degrees-celsius 9) 5)))

"Tests for Celsius->Fahrenheit"

89.6
(Celsius->Fahrenheit 32)
