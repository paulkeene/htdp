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
