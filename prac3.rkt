#lang racket
;;zadanie 1
 
(define (make-rat n d)
  (if (= d 0) false
      (let ((GCD (gcd d n)))
        (cons (/ n GCD) (cons (/ d GCD) null)))))
 
(define (rat-num x)
  (if(rat? x) (car x) false))
 
(define (rat-den x)
  (if (rat? x) (car (cdr x)) false))
 
(define (rat? x)
  (and (pair? x)
       (pair? (cdr x))
       (not (= 0 (car (cdr x))))
       (null? (cdr (cdr x)))))
 
;;bedzie zachodzic
 
;;zadanie 2
 
(define (make-point x y)
  (cons x y))
 
(define (point? a)
  (and (pair? a)
       (not (pair? (car a)))
       (not (pair? (cdr a)))))
 
(define (point-x a)
  (if (point? a) (car a) false))
 
(define (point-y a)
  (if (point? a) (cdr a) false ))
 
(define (make-vect a b)
  (if (and (point? a) (point? b)) (cons a b) false))
 
(define (vect? ab)
  (and (pair? ab)
       (point? (car ab))
       (point? (cdr ab))))
 
(define (vect-begin ab)
  (if (vect? ab) (car ab) false))
 
(define (vect-end ab)
  (if (vect? ab) (cdr ab) false))
 
(define (square x) (* x x ))
 
(define (vect-lenth ab)
  (if (vect? ab)
      (sqrt (+ (square (- (point-x (vect-begin ab))
                          (point-x (vect-end ab))))
               (square (- (point-y (vect-begin ab))
                          (point-y (vect-end ab))))))
      false))
 
 
(define (vect-scale v k)
  (if (vect? v)
      (let ((x0 (point-x (vect-begin v)))
            (y0 (point-y (vect-begin v)))
            (x1 (point-x (vect-end v)))
            (y1 (point-y (vect-end v))))
        (let ((delx (- x1 x0))
              (dely (- y1 y0)))
          (make-vect (vect-begin v)
                     (make-point (+ x0 (* k delx))
                                 (+ y0 (* k dely))))))
      false))
 
 
(define (vect-translate v p)
  (if (and (vect? v) (point? p))
       (let ((x0 (point-x (vect-begin v)))
            (y0 (point-y (vect-begin v)))
            (x1 (point-x (vect-end v)))
            (y1 (point-y (vect-end v))))
        (let ((delx (- x1 x0))
              (dely (- y1 y0)))
          (make-vect p (make-point (+ (point-x p) delx)
                                   (+ (point-y p) dely)))))
      false))
 
 
;;zadanie 3
 
 
(define (make-alt-vect p alfa lenth)
  (if (and (point? p) (>= alfa 0) (< alfa 6.28318) (>= lenth 0))
      (cons p (cons alfa lenth))
      false))
 
(define (is-vector? v)
  (and (pair? v)
       (point? (car v)))
       (pair? (cdr v))
       (not (pair? (car (cdr v))))
       (not (pair? (cdr (cdr v)))))
 
(define (vect-alt-begin v)
  (if (is-vector? v)
      (car v)
      false))
 
(define (vect-alt-alfa v)
  (if (is-vector? v)
      (car (cdr v))
      false))
 
(define (vect-alt-lenth v)
  (if (is-vector? v)
      (cdr (cdr v))
      false))
 
(define (alt-scale v k)
  (if (is-vector? v) 
      (make-alt-vect (vect-alt-begin v)
                     (vect-alt-alfa v)
                     (* k (vect-alt-lenth v)))
      false))

(define (alt-trans v p)
  (if (and (is-vector? v) (point? p)) 
      (make-alt-vect (p)
                     (vect-alt-alfa v)
                     (vect-alt-lenth v))
      false))
 
;;latwe, ale trzebaby doklepac poczatek i koniec wektora, dlugosc, czy wektor, translacje i skalowanie... nużące, łatwe
 
;;zadanie 4
 
(define (reverse items)
 (if (null? items)
     items
     (append (reverse (cdr items)) (list (car items)))))

(define (reverse-iter l)
  (define (iter a r)
    (if (null? a)
        r
        (iter (cdr a) (cons (car a) r))))
  (iter l '()))

;;zadanie 5
(define (make-list s e term)
  (if (> s e) null
      (cons (term s) (make-list (+ s 1) e term))))
 
(define (insert list v)
  (define (rek  tail)
    (if (or (null? tail) (> (car tail) v) )
        (cons v tail)
        (cons (car tail) (rek (cdr tail)))))
    (rek list))

(define (insert2 list v)
    (if (or (null? list) (>= (car list) v) )
        (cons v list)
        (cons (car list) (insert2 (cdr list) v))))
(insert2 (list 1 2) 4)
 
 
(define (insert-sort xs)
    (if (null? xs)
        null
        (insert (insert-sort (cdr xs)) (car xs) )))
 
 
 
;;zadanie 6

(define (add-head v l)
  (define (rek front tail)
    (if (null? tail)
        (cons (append front v) null)
        (cons (append front v tail) (rek (append front (list (car tail))) (cdr tail)))))
  (apply append (map (lambda (x) (rek '() x)) l)))
  

(define (permi s)
  (if (null? s)
      (list null)
      (add-head (list (car s)) (permi (cdr s)))))

;;zad 9
(define (append2 . l)
  (if (null? l)
      null
      (append (car l) (apply append2 (cdr l)))))

(append2 (list 1 2 3) (list 4 5 6) (list 7 8 9))

(define (qsort a)
  (if (empty? a)
    a
    (let ([p (car a)])
      (let ([tail (cdr a)])
        (let ([lsr (filter (lambda (x) (< x p)) tail)])
          (let ([grt (filter (lambda (x) (>= x p)) tail)])
            (append (qsort lsr) (list p) (qsort grt))))))))
