#lang racket

; --- 5.1.1 ---
(define (reply s)
  (cond
    [(symbol=? s 'GoodMorning) 'Hi]
    [(symbol=? s 'HowAreYou?) 'Fine]
    [(symbol=? s 'GoodAfternoon) 'INeedANap]
    [(symbol=? s 'GoodEvening) 'BoyAmITired]))

(symbol=? 'Hi (reply `GoodMorning))
(symbol=? 'Fine (reply `HowAreYou?))
(symbol=? 'INeedANap (reply `GoodAfternoon))
(symbol=? 'BoyAmITired (reply `GoodEvening))


; --- 5.1.2 ---
(define (check-guess guess target)
  (cond
    [(< guess target) 'TooSmall]
    [(> guess target) 'TooLarge]
    [else 'Perfect]))

(symbol=? 'TooSmall (check-guess 1 5))
(symbol=? 'TooLarge (check-guess 9 5))
(symbol=? 'Perfect (check-guess 5 5))


; --- 5.1.3 ---
(define (check-guess3 a b c target)
  (check-guess (to-num a b c) target))

(define (to-num a b c)
  (+ a (* 10 b) (* 100 c)))

(symbol=? 'TooSmall (check-guess3 2 3 0 916))
(symbol=? 'TooLarge (check-guess3 0 2 1 84))
(symbol=? 'Perfect (check-guess3 8 5 1 158))


; --- 5.1.4 ---
(define (what-kind a b c)
  (cond
    [(= a 0) 'degenerate]
    [else (num-solutions a b c)]))

(define (num-solutions a b c)
  (cond
    [(> (* b b) (* 4 a c)) 'two]
    [(= (* b b) (* 4 a c)) 'one]
    [else 'none]))

(symbol=? 'two (what-kind 1 0 -1))
(symbol=? 'one (what-kind 2 4 2))
(symbol=? 'two (what-kind 3 5 0))
(symbol=? 'none (what-kind 4 0 2))
(symbol=? 'degenerate (what-kind 0 3 6))


; --- 5.1.5 ---
(define (check-color target-a target-b guess-a guess-b)
  (cond
    [(and (symbol=? target-a guess-a) (symbol=? target-b guess-b)) 'Perfect]
    [(or (symbol=? target-a guess-a) (symbol=? target-b guess-b))
      'OneColorAtCorrectPosition]
    [(or (symbol=? target-a guess-b) (symbol=? target-b guess-a)) 'OneColorOccurs]
    [else 'NothingCorrect]))

(symbol=? 'Perfect (check-color 'green 'blue 'green 'blue))
(symbol=? 'OneColorAtCorrectPosition (check-color 'green 'blue 'green 'red))
(symbol=? 'OneColorAtCorrectPosition (check-color 'pink 'black 'brown 'black))
(symbol=? 'OneColorOccurs (check-color 'green 'blue 'red 'green))
(symbol=? 'NothingCorrect (check-color 'green 'blue 'yellow 'purple))
