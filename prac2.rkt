#lang racket
(define (square x) (* x x))
(define (inc x) (+ 1 x))
(define (compose f g) (lambda (x) (f (g x))))

;;ZAD 3
(define (repeated p n)
      (if (>= 0 n)
        identity
        (compose p (repeated p (- n 1)))))

;;ZAD 4
(define product-rec
  (lambda (term next a b)
    (if (> a b)
      1.0
      (* (term a)
         (product-rec term next (next a) b)))))

 
(define LIMIT 1000)

(define (mult-pi-alt n e)
  (define (mult-pi-term s)
    (/ (* s (+ s 2)) (square (add1 s))))
  (product-rec mult-pi-term (lambda (s) (+ s 2)) n e))

(define PI (* 4 (mult-pi-alt 2 LIMIT)))



(define (product-iter term next a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1.0))

(define (mult-pi-alt2 n e)
  (define (mult-pi-term s)
    (/ (* s (+ s 2)) (square (add1 s))))
  (product-iter mult-pi-term (lambda (s) (+ s 2)) n e))

(define _PI (* 4 (mult-pi-alt2 2 LIMIT)))

PI

_PI

(newline)

;;ZAD 5
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
 
(define (sum-acc a b)
  (accumulate + 0 identity a add1 b))
 
(define (product-acc a b)
  (accumulate * 1 identity a add1 b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-acc-iter a b)
  (accumulate-iter + 0 identity a add1 b))
 
(define (product-acc-iter a b)
  (accumulate-iter * 1 identity a add1 b))

(sum-acc 1 5)
(sum-acc-iter 1 5)

(newline)
;;ZAD 6
(define (cont-frac n d k i);!!! tu był błąd na tablicy jak z P. Charatonikiem ustaliliśmy.
  (if (= i k)
      1
      (/ (n i) (+ (d i) (cont-frac n d k (add1 i))))))

(define (cont-frac-iter n d k)
  (define (iter k result)
    (if (zero? k)
        result
        (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

(cont-frac (lambda(i) 1.0) (lambda(i) 1.0) 42 0)
(cont-frac-iter (lambda(i) 1.0) (lambda(i) 1.0) 42)

(newline)
;;ZAD 7
(define (pi-cf k)
  (+ (cont-frac (lambda (i)
                    (square (- (* i 2) 1)))
                  (lambda (i) 6.0)
                  k 1) 3))

(define (cout a b)
  (display "k:")
  (display a)
  (newline)
  (display b))

(define (__PI)
  (define (iter k)
    (let ((pi-k (pi-cf k)))
      (if (< (abs (- pi-k pi)) 0.00001)
          (cout k pi-k)
          (iter (add1 k)))))
  (iter 1))
(__PI)

(newline)
(newline)
;;ZAD 8
(define (atan-cf x k)
  (cont-frac-iter (lambda (i) 
                    (if (= i 1) x (square (* (- i 1) x))))
                  (lambda (i) 
                    (- (* 2 i) 1))
                  k))
(atan-cf 0.5 1000)
(atan 0.5)

(newline)
;;ZAD 9
(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  ((repeated (lambda (x) (build n d x)) k) b))
 
(repeated-build 2 1 1 1)
(build 1 1 (build 1 1 1))
