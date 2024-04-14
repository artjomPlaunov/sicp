#lang racket

(define (runtime) (current-inexact-milliseconds)) ; adapting to DrRacket

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (cond  ((= n 2) 3)
         (else (+ n 2))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (cond ((= n 1) #f)
        (else (= n (smallest-divisor n)))))

;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      "nothing"))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b n)
  (cond ((even? a) (search-aux (+ a 1) b n))
        (else (search-aux a b n))))

(define (search-aux a b n)
  (cond ((or (= n 0) (> a b)) (void))
        (else (timed-prime-test a)
              (cond ((prime? a) (search-aux (+ a 2) b (- n 1)))
                    (else (search-aux (+ a 2) b n))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random(- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (carmichael-aux n a test)
  (cond ((= a 0) test)
        (else
         (and test (= (expmod a n n) a)))))

(define (carmichael-test n)
  (carmichael-aux n (- n 1) #t))

;Smallest Few Carmichael Numbers:
(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)



        