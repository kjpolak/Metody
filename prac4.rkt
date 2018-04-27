#lang racket
;;; drzewa binarne

(define (leaf? x)
  (eq? x 'leaf))

(define leaf 'leaf)

(define (node? x)
  (and (list? x)
       (= (length x) 4)
       (eq? (car x) 'node)))

(define (node-val x)
  (cadr x))

(define (node-left x)
  (caddr x))

(define (node-right x)
  (cadddr x))

(define (make-node v l r)
  (list 'node v l r))

(define (tree? t)
  (or (leaf? t)
      (and (node? t)
           (tree? (node-left t))
           (tree? (node-right t)))))

;;; wyszukiwanie i wstawianie w drzewach przeszukiwań binarnych

(define (bst-find x t)
  (cond [(leaf? t)          false]
        [(= x (node-val t)) true]
        [(< x (node-val t)) (bst-find x (node-left t))]
        [(> x (node-val t)) (bst-find x (node-right t))]))

(define (bst-insert x t)
  (cond [(leaf? t)
         (make-node x leaf leaf)]
        [(< x (node-val t))
         (make-node (node-val t)
                    (bst-insert x (node-left t))
                    (node-right t))]
        [else
         (make-node (node-val t)
                    (node-left t)
                    (bst-insert x (node-right t)))]))


(define(btree? t)
  (or(eq? t 'leaf)
     (and (list? t)
          (= 4 (length t))
          (eq? (car t) 'node)
          (btree? (caddr t))
          (btree? (cadddr t)))))

(define (mirror tree)
  (if (eq? tree 'leaf)
      tree
      (list 'node (cadr tree) (mirror (cadddr tree)) (mirror (caddr tree)))))

(define (flatten tree)
  (define (flattend tree ans)
    (if (eq? tree 'leaf)
        ans
        (flattend (caddr tree) (cons (cadr tree) (flattend (cadddr tree) ans)))))
  (flattend tree null))

(define (treesort xs)
  (define (list->tree xs tree)
    (if (null? xs)
        tree
        (list->tree (cdr xs) (bst-insert (car xs) tree))))
  (flatten (list->tree xs 'leaf)))

(define (bst-min tree)
  (if (eq? 'leaf (node-left tree))
      (node-val tree)
      (bst-min (node-left tree))))

(bst-min (make-node 3 'leaf 'leaf))
(define (delete tree x)
  (cond [(null? tree) null]
        [(= x (node-val tree))
         (if (eq? 'leaf (node-right tree))
             (node-left tree)  
             (let ([min (bst-min (node-right tree))])
               (make-node min (node-left tree) (delete (node-right tree) min))))]
        [(> x (node-val tree))
         (make-node (node-val tree) (node-left tree) (delete (node-right tree) x))]
        [else (make-node (node-val tree) (delete (node-left tree) x) (node-right tree))]))
