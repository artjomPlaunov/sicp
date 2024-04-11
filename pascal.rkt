#lang racket

(define (comb n k)
  (cond ((= n 0) 0)
        ((= k 0) 1)
        ((= n k) 1)
        (else (+ (comb (- n 1) (- k 1)) (comb (- n 1) k)))))

