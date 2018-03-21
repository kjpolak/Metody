#lang racket
(define (var? t);zmienna
  (symbol? t))

(define (neg? t);negacja(znak negacji(lub jego brak). zmienna)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t);koniunkcja(znak koniunkcji. lewa część formuły, prawa część)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (disj? t);alternatywa
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (prop? f);czy formuła jest poprawną formułą rachunku zdań
  (or (var? f);czyli 1.każda zmienna f jest poprawną formułą rachunku zdań
      (and (neg? f);2. jeżeli f, f, f2 są formułami rachunku zdań to są nimi również
           (prop? (neg-subf f)));(neg f), (conj f1 f2), (disj f1 f2)
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-rght f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-rght f)))))

;;;Ćwiczenie 1;;;
(define (neg zmienna);czy należy sprawdzić predykat var?
  (list 'neg zmienna))

(define (neg-subf formuła)
  (cdr formuła))

(define (conj formuła1 formuła2)
  (list 'conj formuła1 formuła2))

(define (conj-left formuła)
  (second formuła))

(define (conj-rght formuła)
  (third formuła))

(define (disj formuła1 formuła2)
  (list 'disj formuła1 formuła2))

(define (disj-left formuła)
  (second formuła))

(define (disj-rght formuła)
  (third formuła))

;;;Ćwiczenie2;;;

(define (free-vars formuła)
  (define (helper acc)
    (if (null? formuła)
        acc
        (if (var? (car formuła))            ;;;and (not (już_zapisana? formuła acc)))
            (cons formuła acc)
            (cond [(neg? formuła) (free-vars (neg-subf formuła))]
                  [(conj? formuła) (cons (free-vars (conj-left formuła)) (free-vars (conj-rght formuła)))];cons nie bardzo tu pasuje
                  [(disj? formuła) (cons (free-vars (disj-left formuła)) (free-vars (disj-rght formuła)))]))))
  (helper '()))


(define (free-vars1 f)
  (cond ((var? f) f)
        ((neg? f) (free-vars1 (neg-subf f)))
        ((conj? f) (cons (free-vars1 (conj-left f))
                         (free-vars1 (conj-rght f))))
        ((disj? f) (cons (free-vars1 (disj-left f))
                         (free-vars1 (disj-rght f))))))

(define (już_zapisana? zmienna lista)
  (cond ([(null? lista) '(#f)]
         [(eq? zmienna (car lista)) #t]
         [(już_zapisana? zmienna (cdr lista))])))

;(free-vars1 (list 'conj (list 'disj 'p 'q) 'q))

(define (gen-vals xs)
  (if ( null? xs )
    ( list null )
    ( let*
         (( vss ( gen-vals ( cdr xs ) ) )
         ( x ( car xs ) )
         ( vst ( map ( lambda ( vs ) ( cons ( list x true ) vs ) ) vss ) )
         ( vsf ( map ( lambda ( vs ) ( cons ( list x false ) vs ) ) vss ) ) )
      ( append vst vsf ) ) ) )
;(gen-vals (list 'p 'q))

(define (eval-formula formula wartosciowanie)
  (cond ([(var? formula) (bool formula wartosciowanie)]
         [(conj? formula) (and (eval-formula (conj-left formula) wartosciowanie) (eval-formula (conj-rght formula) wartosciowanie))]
         [(disj? formula) (and (eval-formula (disj-left formula) wartosciowanie) (eval-formula (disj-rght formula) wartosciowanie))])))

(define (bool zmienna wartościowanie)
  (cond([(eq? (car (car wartościowanie)) zmienna) (car (cdr wartościowanie))]
        [(null? wartościowanie) (error "Zmienna nie występuje w podanej liście wartośiowań")]
        [(< 0 1) (bool zmienna (cdr wartościowanie))])))
(eval-formula (list 'disj 'p 'q) '((p #t) (q #t)))

(define (free-vars1 f)
  (cond ((var? f) f)
        ((neg? f) (free-vars1 (neg-subf f)))
        ((conj? f) (cons (free-vars1 (conj-left f))
                         (free-vars1 (conj-rght f))))
        ((disj? f) (cons (free-vars1 (disj-left f))
                         (free-vars1 (disj-rght f))))))

(define (już_zapisana? zmienna lista)
  (cond ([(null? lista) '(#f)]
         [(eq? zmienna (car lista)) #t]
         [(już_zapisana? zmienna (cdr lista))])))

;(free-vars1 (list 'conj (list 'disj 'p 'q) 'q))
;;;Zadanie3;;;
(define (gen-vals xs)
  (if ( null? xs )
    ( list null )
    ( let*
         (( vss ( gen-vals ( cdr xs ) ) )
         ( x ( car xs ) )
         ( vst ( map ( lambda ( vs ) ( cons ( list x true ) vs ) ) vss ) )
         ( vsf ( map ( lambda ( vs ) ( cons ( list x false ) vs ) ) vss ) ) )
      ( append vst vsf ) ) ) )
;(gen-vals (list 'p 'q))

(define (eval-formula formula wartosciowanie)
  (cond ([(var? formula) (bool formula wartosciowanie)]
         [(neg? formula) (not (eval-formula (neg-subf formula)))]
         [(conj? formula) (and (eval-formula (conj-left formula) wartosciowanie) (eval-formula (conj-rght formula) wartosciowanie))]
         [(disj? formula) (and (eval-formula (disj-left formula) wartosciowanie) (eval-formula (disj-rght formula) wartosciowanie))])))

#|(define (bool zmienna wartościowanie);;;lub procedura assoc - robi dokładnie prawie to samo
  (cond([(eq? (car (car wartościowanie)) zmienna) (car (cdr wartościowanie))]
        [(null? wartościowanie) (error "Zmienna nie występuje w podanej liście wartośiowań")]
        [(< 0 1) (bool zmienna (cdr wartościowanie))])))
|#

(define (bool zmienna wartosciowanie)
  (if (null? wartosciowanie)
      (error "error")
      (if (eq? (car (car wartosciowanie)) zmienna)
          (cdr (car wartosciowanie))
          (bool zmienna (cdr wartosciowanie)))))

(define (falsifiable-eval? f)
  (define (iter vals)
    (if (null? vals)
        #f
        (if (eval-formula f (car vals))
            (iter (cdr vals))
            (car vals))))
  (iter (gen-vals (free-vars f))))
;(eval-formula (list 'disj 'p 'q) '((p #t) (q #t)))

;;;Ćwiczenie4;;;

(define (literal p)
  (if (or (var? p)
          (and (neg? p)
               (var? (neg-subf p))))
      (list 'literal p)
      (error "brak")))

(define (literal? l)
  (and (list? l)
       (= 2 (length l))
       (eq? (car l) 'literal)
       (or (var? (second l))
           (and (neg? (second l))
                (var? (neg-subf (second l)))))))
#|(define (nnf? f)
  (cond ([(literal? f) #t]
         [(conj? f) (and (nnf (conj-left f)) (nnf (conj-rght f)))]
         [(disj? f) (or (nff (disj-left f)) (nnf (disj-rght f)))]
         [(neg? f) #f]
         [else (error "Podano nieprawidłowa formule")])))
|#
;;;Ćwiczenie5;;;

(define (convert-to-nnf f)
  (cond ([(var? f) (literal f)]
         [(neg? f) (convert-to-nnf-neg (neg-subf f))]
         [(conj? f) (conj (convert-to-nnf (conj-left)) (convert-to-nnf (conj-rght)))]
         [(disj? f) (disj (convert-to-nnf (disj-left)) (convert-to-nnf (disj-rght)))])))

(define (convert-to-nnf-neg f)
  (cond ([(var? f) (literal (neg f))]
         [(disj? f) (conj (convert-to-nnf (disj-left f)) (convert-to-nnf (disj-rght f)))]
         [(conj? f) (disj (convert-to-nnf (conj-left f)) (convert-to-nnf (conj-rght f)))]
         [(neg? f) (convert-to-nnf (neg-subf f))])))
(convert-to-nnf (list 'f))

;;;Ćwiczenie6;;;

(define (klazula lista_klauzul)
  (cons 'cl lista_klauzul))

(define (klauzula? f)
  (and? (eq? (car f) 'cl)
        (pair? f)
        (list? (cdr f))
        (andmap literal? (cdr f))))

(define (cnf? f)
  (and (pair? f)
       (eq? (car f) 'cnf)
       (list? (cdr f))
       (andmap cl? (cdr f))))

(define (cnf klauzule)
  (cons 'cnf klauzule))

(define (convert-to-cnf f)
  (cond ([(literal? f) (cnf (list (klauzula (list f))))]
         [(conj? f) (append (cdr (convert-to-cnf (conj-left f))) (cdr (convert-to-cnf (conj-rght f))))];tag cnf?
         [(disj? f) (cnf (join (cdr (disj-left f)) (cdr (disj-rght f))))])))

(define (join xss yys)
  (apply append(map (lambda (p) (map (lambda (q) (append p (cdr q))yss) xss)))))




