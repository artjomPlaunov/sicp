#lang racket

(define (fast-expt b n)
  (fast-expt-aux 1 b n))

(define (fast-expt-aux a b n)
  (cond ((= 0 n) a)
        ((= 2 n) (* a b b))
        ((even? n) (fast-expt-aux (* a b b) b (/ n 2)))
        (else (fast-expt-aux (* a b) b (- n 1)))))

(define (double n)
  (* 2 n))

;; This procedure assumes n is even!
(define (halve n)
  (/ n 2))

; mul procedure that only uses double and halve operations.
; logarithmic in size of n.
; recursive process
(define (mul a b)
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (mul a (- b 1))))))

; iterative procedure,
; using the fact that a*b = 2a*(b/2), for even b.
; for odd b, a*b = a + (a*(b-1)), so we add a to the
; accumulator and compute a*(b-1).

(define (mul-fast a b)
  (mul-fast-aux 0 a b))

(define (mul-fast-aux acc a b)
  (cond ((= b 0) acc)
        ((even? b) (mul-fast-aux acc (double a) (halve b)))
        (else (mul-fast-aux (+ acc a) a (- b 1)))))


