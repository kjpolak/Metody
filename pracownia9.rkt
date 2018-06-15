

#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))
;;

(define (node l r)
  (list 'node l r))

(define (node? n)
  (tagged-tuple? 'node 3 n))

(define (node-left n)
  (second n))

(define (node-right n)
  (third n))

(define (leaf? n)
  (or (symbol? n)
      (number? n)
      (null? n)))

;;

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;;

(define (rename t)
  (define (rename-st t i)
    (cond [(leaf? t) (res i (+ i 1))]
          [(node? t)
           (let* ([rl (rename-st (node-left t) i)]
                  [rr (rename-st (node-right t) (res-state rl))])
             (res (node (res-val rl) (res-val rr))
                  (res-state rr)))]))
  (res-val (rename-st t 0)))

;;

(define (st-app f x y)
  (lambda (i)
    (let* ([rx (x i)]
           [ry (y (res-state rx))])
      (res (f (res-val rx) (res-val ry))
           (res-state ry)))))

;;st-app wieloargumentowy zadanie 1
#|
(define (st-app f . args)
  (define (helper xs i)
    (if (null? xs)
        null
        (let ((result ((cdr xs) i)))
          (cons result (helper (cdr xs) (res-state result))))))
  (lambda (i)
    (let ((result (helper args i)))
      (res (apply f (map res-val result)) (res-state (last result))))))|#

(define get-st
  (lambda (i)
    (res i i)))

(define (modify-st f)
  (lambda (i)
    (res null (f i))))

(define tree (node (node 0 0) (node 0 0)))


;;

(define (inc n)
  (+ n 1))

(define (rename2 t)
  (define (rename-st t)
    (cond [(leaf? t)
           (st-app (lambda (x y) x)
                   get-st
                   (modify-st inc))]
          [(node? t)
           (st-app node
                   (rename-st (node-left  t))
                   (rename-st (node-right t)))]))
  (res-val ((rename-st t) 0)))

;;(rename2 tree)
;; zadanie 2
       
(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

(define (rename3 t)
  (define (rename-st t)
    (cond [(leaf? t)
           (st-app (lambda (x y) x)
                   (rand max)
                   (modify-st (lambda (x) x)))]
          [(node? t)
           (st-app node
                   (rename-st (node-left  t))
                   (rename-st (node-right t)))]))
  (res-val ((rename-st t) 0)))

;;(rename3 tree)

;;
;; WHILE
;;

; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bools

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]))

(define (var? t)
  (symbol? t))

(define (eval-arith e m)
  (cond [(var? e) (get-mem e m)]
        [(op? e)
         (apply
          (op->proc (op-op e))
          (map (lambda (x) (eval-arith x m))
               (op-args e)))]
        [(const? e) e]))

;;

(define (++? t)
  (and (list? t)
       (= 2 (length t))
       (symbol? (car t))
       (symbol? (cadr t))
       (or (eq? (car t) '++)
           (eq? (cadr t) '++))))

(define (++-var e)
  (if (eq? (car e) '++)
      (cadr e)
      (car e)))

(define (--? t)
  (and (list? t)
       (= 2 (length t))
       (symbol? (car t))
       (symbol? (cadr t))
       (or (eq? (car t) '--)
           (eq? (cadr t) '--))))

(define (---var e)
  (if (eq? (car e) '--)
      (cadr e)
      (car e)))

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (for? t)
  (tagged-tuple? 'for 5 t))

(define (for-assign t)
  (second t))

(define (for-cond t)
  (third t))

(define (for-inc t)
  (fourth t))

(define (for-expr t)
  (fifth t))

(define (block? t)
  (list? t))

;;
;;

(define (eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (eval-arith (if-cond e) m)
             (eval (if-then e) m)
             (eval (if-else e) m))]
        [(while? e)
         (if (eval-arith (while-cond e) m)
             (eval e (eval (while-expr e) m))
             m)]
        [(for? e) (eval (list (for-assign e) (list 'while (for-cond e) (list (for-expr e) (for-inc e)))) m)]
        [(++? e) (set-mem (++-var e) (+ (get-mem (++-var e) m) 1)m)]
        [(--? e) (set-mem (---var e) (- (get-mem (---var e) m) 1)m)]
        [(block? e)
         (if (null? e)
             m
             (eval (cdr e) (eval (car e) m)))]))

(define (run e)
  (eval e empty-mem))

;;

(define fact10
  '( (i := 10)
     (r := 1)
     (while (> i 0)
       ( (r := (* i r))
         (i := (- i 1)) ))))

(define (computeFact10)
  (run fact10))

(define fib10
  '((n := 10)
    (f1 := 0)
    (f2 := 1)
    (while (> n 0)
           ((tmp := f2)
           (f2 := (+ f1 f2))
           (f1 := tmp)
           (n := (- n 1))))))

(define (fib1 k)
  '((n := ,k)
    (f1 := 0)
    (f2 := 1)
    (while (> n 0)
           ((tmp := f2)
           (f2 := (+ f1 f2))
           (f1 := tmp)
           (n := (- n 1))))))


(define (fib2 k)
  '((list (append (n :=) (list k)))
    (f1 := 0)
    (f2 := 1)
    (while (> n 0)
           ((tmp := f2)
           (f2 := (+ f1 f2))
           (f1 := tmp)
           (n := (- n 1))))))


(define (liczfib)
  (run fib10))

;(computeFact10)
;(liczfib)
#|
(define (primes n)
  '((sum := 0)
    (n := ,n)
    (i := 2)
    (while (> n 0)
           ((divs := 0)
            (j := 2)
            (while (<= (* j j) i)
                   ((if (= (i j) 0)
                        (divs := (+ divs 1))
                        ())
                    (if (> divs 0)
                        ()
                        ((sum := (+ i sum))
                         (n := (- n 1))
                         ))))))))|#


(define prog
  '((n := -4)
    (while (< n 0)
           ((n ++)))))
(run prog)

(define prog2
  '((for ((i := -4)
          (j := 0)) (< i 0) (i ++)(j ++))))
(run prog2)

(define (translate e)
  (cond [(assign? e) e]
        [(if? e)
         (list 'if (if-cond e) )
         (translate (if-then e))
         (translate (if-else e))]
        [(while? e)
         (list 'while (while-cond e) (translate (while-expr e)))]
        [(for? e) (list (for-assign e) (list 'while (for-cond e) (list (for-expr e) (for-inc e))))]
        [(block? e)
         (if (null? e)
             null
             (cons (translate (car e)) (translate (cdr e))))]))
(run (translate prog2))
