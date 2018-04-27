#lang racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;(a-plus-abs-b 4 -2)

(define (power-close-to x y)
  (define (dodaj z) (+ z 1))
  (define (szukaj z)
    (if (> (expt x z) y) z (szukaj (dodaj z))))
  (szukaj 1))
;(power-close-to 2 1000)

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;(test 0 (p))

(define (fib a b n)
  (if (= n 0)
      b
      (fib (+ b a) a (- n 1))))
;(fib 1 0 10)

(define (memq item x)
  (cond [(null? x) #f]
        [(eq? item (car x)) x]
        [else (memq item (cdr x))]))

(memq 'apple '(x (apple sause) y apple pear))

(map + (list 1 2 3) (list 1 2 3) (list 1 2 3))
