#lang racket

;; reverse a list
(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst acc)
  (if (null? lst)
      acc
      (reverse-aux (cdr lst)
                   (cons (car lst) acc))))

;; checking lambda symbol
(define square (λ (x) (* x x)))

(map square (reverse '(1 2 3)))

;; composition function
(define (comp x y)
  (λ (a)
    (y (x a))))

(map (comp square square) (reverse '(4 5 6)))

(define inc add1)
(define dec sub1)

((compose inc (λ (x) (* 2 x))) 42)

;; example of evaling rator
((if (null? '(1))
     square
     (comp (comp inc inc) square))
 42)

;; talking about evaluation models
;; normal-order & applicative-order
#;(sum-of-squares (+ 1 2) (+ 3 4))
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

;; closure example
(define (counter next)
  (λ ()
    (let ([return next])
      (set! next (add1 next))
      return)))

(define p (counter 0))
(p)
(p)

;; test of evaluation model
;; if terminates - we're in normal-order
(define (f) (f))
(define (test x y)
  (if (= x 0) 0 y))
#;(test 0 (f))

(define epsilon 0.000001)
(define start-point 1.0)
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) epsilon))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (sqrt-iter start-point))

(sqrt 42)

;; recursion
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
(time (factorial 42))
;;linear recursive expansion
#;((λ (x) (* x _)) 42)
#;(* 42 (* 41 (_)))

(define (factorial2 n)
  (define (factorial-iter i a)
    (if (> i n)
        a
        (factorial-iter (inc i)
                        (* i a))))
  (factorial-iter 1 1))
(time (factorial2 42))
;;linear iterative expansion
;;1 1 -> 2 1 -> 3 2 -> 4 6
(factorial 3)

;;factorial2 describes iterative process using recursion syntaxis

;;iterative or recursive?
(define (plus a b)
  (if (= a 0) b (inc (plus (dec a) b))))
(define (plus2 a b)
  (if (= a 0) b (plus2 (dec a) (inc b))))

;; ackermann
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(A 3 3)
