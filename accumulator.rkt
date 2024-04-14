#lang racket

;prime code
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
; end prime code

(define (cube n) (* n n n))

(define (inc n) (+ n 1))

(define (identity x) x)

; recursive sum
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

; iterative sum
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ result (term a)))))
;  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (y_k k) (f (+ a (* k h))))
  (define (t k)
    (cond ((= 0 k) (y_k k))
          ((= n k) (y_k k))
          ((even? k) (* 2 (y_k k)))
          (else (* 4 (y_k k)))))
  (* (/ h 3) (sum t 0 inc n)))

;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a) (product term (next a) next b))))

;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (* result (term a)))))
;  (iter a 1))

(define (factorial n)
  (if (= n 0)
      1
      (product identity 1 inc n)))

(define (approx-pi n)
  (define (k n) (cond ((even? n) (+ 4 (* 2 (/ (- n 2) 2))))
                         (else (+ 4 (* 2 (/ (- (- n 1) 2) 2))))))
                 
  (define (t n)
    (let ([z (k n)])
    (cond ((= n 1) (/ 2 3))
          (else (cond ((even? n) (/ z (- z 1)))
                       (else (/ z (+ z 1))))))))
  (* 4.0 (product t 1 inc n)))

;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (term a)
;                (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (acc-iter a result)
    (if (> a b)
        result
        (acc-iter (next a) (combiner result (term a)))))
  (acc-iter a null-value))
  

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (filtered-acc pred comb null term a next b)
  (define (acc-iter a result)
    (if (> a b)
        result
        (if (pred a)
            (acc-iter (next a) (comb result (term a)))
            (acc-iter (next a) result))))
  (acc-iter a null))

(define (sum-prime-squares a b)
  (filtered-acc prime? + 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gen-is-coprime n)
  (lambda (a) (= 1 (gcd a n))))

(define (product-coprimes n)
  (filtered-acc (gen-is-coprime n) * 1 identity 1 inc (- n 1)))
  


              





