#lang racket

; --- 8.2.1 ---
; x is legal because it represents a variable and a lone variable is
; a valid expression
;
; (= y z) is a valid expression because = is a primitive function, y
; and z are variables and the function is being properly invoked by
; being places between parentheses
;
; (= (= y z) 0) is a valid expression because the inner expression
; is well-formed according to the scheme grammer (i.e. function
; invocation in-between parentheses with arguments) and so is the
; outer expression (function invocation inside parentheses where
; one argument is an expression and the other is a constant). Note:
; This will generate a runtime error since the = function expects
; numerical arguments and (= y z) will produce a boolean result
;
; (3 + 4) is an illegal expression because function invocation
; requires that the function name comes after the opening parenthesis
; and it is followed by the arguments being passed to the function
;
; empty?(l) is an illegal expression because empty? is a function
; and therefore needs to be inside the first parenthesis instead
; of outside
;
; (x) is an illegal expression because if x is a function then it
; is being invoked with no arguments, which isn't allowed in scheme.
; If it's not a function then it shouldn't be put inside parentheses
; since they are reserved for function invocation, definitions, and
; conditionals.


; --- 8.2.2 ---
; (define (f x) x) is a valid definition because it has the correct form:
; the parentheses are in the right place the define keyword is followed by
; variable names enclosed in parentheses and an expression (x).
;
; (define (f x) y) is valid for the same reson as (define (f x) y). The
; compiler doesnt know until runtime that y is an unbound variable.
;
; (define (f x y) 3) is valid for the same reason as the above 2 definitions.
; Just because a function is pased arguments does not mean it needs to use them.
;
; (define (f 'x) x) is an illegal definition because 'x is a symbol and the
; scheme grammar dictates that only variables are allowed to be enclosed in the
; parentheses following the define keyword
;
; (define (f x y z) (x)) is an illegal definition because (x) is not a legal
; expression.
;
; (define (f) 10) is not a valid definition because the scheme grammar dictates
; that a minimum of two variables must be specified in the parenthesized
; expression after the define keyword.


; --- 8.2.3 ---
; (x) is an illegal expression because parentheses signify function
; invocation, yet no arguments are being passed to function x.
;
; (+ 1 (not x)) is a legal expression that will fail at runtime. It is
; legal because the outer expression is a function invocation that is passed
; two expressions: a constant and another expression that is a well-formed
; function invocation.
;
; (+ 1 2 3) is a legal expression. It is legal because function invocation
; requires parentheses followed by a function name and 2 or more parameters.
; This expression meets those requirements.


; --- 8.2.4 ---
; (define (f x) 'x) is a legal expression because the define keyword is
; followed by a parenthesized expression of 2 variables, which is followed
; by a valid expression: a constant symbol.
;
; (define (f 'x) x) is not a legal expression because the parenthesized
; expression following define keyword is supposed to be made up of variables
; but 'x is a constant symbol.
;
; (define (f x y) (+ 'y (not x))) is a legal expression because the define
; keyword is followed by a parenthesized expression of 3 variables and a legal
; expression. (+ 'y  (not x)) is a legal function invocation, but it will
; fail during runtime since a symbol cannot be added to a boolean.


; --- 8.3.1 ---
; 1. (+ (* (/ 12 8) 2/3)
;       (- 20 (sqrt 4)))
;    (+ (* 12/8 2/3)
;       (- 20 2))
;    (+ 1
;       18)
;    19
; 2. (cond
;      [(= 0 0) false]
;      [(> 0 1) (symbol=? 'a 'a)]
;      [else (= (/ 1 0) 9)])
;    (cond
;      [true false]
;      [(> 0 1) (symbol=? 'a 'a)]
;      [else (= (/ 1 0) 9)])
;    false
; 3. (cond
;      [(= 2 0) false]
;      [(> 2 1) (symbol=? 'a 'a)]
;      [else (= (/ 1 2) 9)])
;    (cond
;      [false false]
;      [true (symbol=? 'a 'a)]
;      [else (= (/ 1 2) 9)])
;    (cond
;      [true (symbol=? 'a 'a)]
;      [else (= (/ 1 2) 9)])
;    (symbol=? 'a 'a)
;    true


; --- 8.3.2 ---
; f : number number -> number
; (define (f x y)
;   (+ (* 3 x) (* y y)))
;
; 1. (+ (f 1 2) (f 2 1))
;    (+ (+ (* 3 1) (* 2 2))
;       (+ (* 3 2) (* 1 1)))
;    (+ (+ 3 4)
;       (+ 6 1))
;    (+ 7
;       7)
;     14
; 2. (f 1 (* 2 3))
;    (+ (* 3 1) (* (* 2 3) (* 2 3)))
;    (+ 3 (* 6 6))
;    (+ 3 36)
;    39
; 3. (f (f 1 (* 2 3)) 19)
;    (f (f 1 (* 2 3)) 19)
;    (f (+ (* 3 1) (* (* 2 3) (* 2 3))) 19)
;    (+ (* 3 (+ (* 3 1) (* (* 2 3) (* 2 3)))) (* 19 19))
;    (+ (* 3 (+ 3 (* 6 6))) 361)
;    (+ (* 3 (+ 3 36)) 361)
;    (+ (* 3 39) 361)
;    (+ 117 361)
;    478


; --- 8.6.1 ---
; (define MOVIE-STAR 'JeffBridges)
; (define ANSWER (+ 42 0))
; (define AREA (* pi (sqr RADIUS)))
; (define RESOURCE-AVAILABLE (and (not BUSY) (is-open CONNECTION)))
; (define THRESHOLD 33.3)


; --- 8.6.2 ---
; (define RADIUS 10)
;
; (define DIAMETER (* 2 RADIUS))
; (define DIAMATER 20)
;
; (define CIRCUMFERENCE (* 3.14 DIAMETER))
; (define CIRCUMFERENCE 62.8)


; --- 8.6.3 ---
; (define PRICE 5)
;
; (define SALES-TAX (* .08 PRICE)
; (define SALES-TAX .4)
;
; (define TOTAL (+ PRICE SALES-TAX))
; (define TOTAL 5.4)


; --- 8.7.1 ---
; 1. (define-struct personnel-record (name salary dob ssn))
; This is a legal scheme expression because the define-struct keyword
; is followed by a variable which is followed by variables contained
; in parentheses.
;
; 2. (define-struct oops ())
; This is not a legal scheme expression because the parentheses following
; oops do not contain at least one variable name.
;
; 3. (define-struct child (dob date (- date dob)))
; This is not a legal scheme expression because (- date dob) is not a
; legal variable name. The only thing that is allowed to occur in between
; the parenthese following the struct's name is variable names.
;
; 4. (define-struct (child person) (dob date))
; This is not a legal scheme expression. First, a valid define-struct
; statement requires that a variable name follows the define-struct
; keyword, but here we have (child person). Secondly, only one set
; of variables enclosed in parentheses is allowed in a define-struct
; statement.
;
; 5. (define-struct child (parents dob date))
; This is a legal scheme expression. The define-struct keyword is
; followed by one variable name and one set of variables enclosed
; in parentheses.


; --- 8.7.2 ---
; 1. (make-point 1 2 3)
; 2. (make-point (make-point 1 2 3) 4 5)
; 3. (make-point (+ 1 2) 3 4)
;
; All of these are values, although #2 would most likely violate the
; data definition of a point.


; --- 8.7.3 ---
; (define-struct ball (x y speed-x speed-y))
;
; 1. (number? (make-ball 1 2 3 4)) evaluates to false
; 2. (ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3)) evaluates to 3
; 3. (ball-y (make-ball (+ 1 2) (+ 3 3) 2 3)) evaluates to 6
;
; 1. (number? (make-ball 1 3 4)) will produce a runtime error since
;    only 3 arguments are passed to to the make-ball constructor
; 2. (ball-x (make-posn 1 2)) this will produce a runtime error since
;    the ball-x selector expects a ball structure to be passed to it,
;    but instead is receiving a posn structure
; 3. (ball-speed-y 5) will produce a runtime error since the ball-speed
;    selector expects a ball structure to be passed to it, but instead
;    is passed a number
