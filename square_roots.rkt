#lang racket

(define (sum-squares x y)
  (+ (* x x) (* y y)))

(define (sum-squares-majority x y z)
  (cond ((and (>= x y) (>= z y)) sum-squares x z)
        ((and (>= x z) (>= y z)) sum-squares x y)
        (else (sum-squares y z))))

(sum-squares-majority 3 4 5)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (<
   (abs (- 1.0 (/ guess (improve guess x))))
   .000000000001))


(sqrt .0000000000000000000000000000000000000000001)

(define (cubert x)
  (cubert-iter 1.0 x))

(define (cubert-iter guess x)
  (if (good-enough-cubert? guess x)
      guess
      (cubert-iter (improve-cubert guess x)
                   x)))

(define (good-enough-cubert? guess x)
  (<
   (abs (- 1.0 (/ guess (improve-cubert guess x))))
   .0000000000000000001))

(define (improve-cubert guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(cubert 27)
(cubert 147)











