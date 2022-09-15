#lang racket

;;;;;;;;;;;;;;;;;;;;;Zadanie 4;;;;;;;;;;;;;;;;;;;;;;;


;;rekurencyjnie
(define (rev l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))

;;iteracyjnie
(define (rev-i l)
  (define (rev-help l acc)
    (if (null? l)
        acc
        (rev-help (cdr l) (cons (car l) acc))))
  (rev-help l '()))
      

(rev '( 1 2 3 4 5))
(rev-i '(1 2 3 4 5))

;;;;;;;;;;;;;;;;;;Zadanie5;;;;;;;;;;;;;;;;;
;;


;; rekurencyjny
(define (insert xs n)
  (if (null? xs)
      (list n)
      (if (< n (car xs)) ;;pierwszy element ogona
          (append  (list n) xs)
          (append (list (car xs)) (insert (cdr xs) n)))))

;; iteracyjnie
;(define (insert-iter xs n)
;  (define (iter xs n acc)
;    (if (null? xs) (list n)
 ;       (if (< n (car xs)) ;;pierwszy element ogona
;            (cons  (list n) xs)
;            ((insert (cdr xs) n) (cons acc (insert (car xs) n))))))
;  (iter xs n '()))


;(define (insert xs n)
 ; (if (null? xs) (list n)
  ;    (if (< n (car xs)) ;;pierwszy element ogona
   ;       (append  (list n) xs)
    ;      (append (list (car xs)) (insert (cdr xs) n)))))


(define (ins-sort xs)
  (define (ins-s-a ss xs)
    (if (null? xs) ss
        (ins-s-a (insert ss (car xs)) (cdr xs))))
  (ins-s-a (list) xs))

;;;;;;;;;;;;;FOLDL ==> ZADANIE 7

;;(reverse (append xs ys) == (append (reverse ys) (reverse xs))
;;(map f ( append xs yx)) == (append (map f? xs) (map f? ys))
;;(filter p? (append xs yx)) == (append (filter p? xs) (filter p? ys)


;;append jest łączny, a jego el neytralny to lista pusta 

(insert '( 1 2 4 5 8) 3)
;(insert-iter '( 1 2 4 5 8) 9 )

;;filter wywala wsztsrko co nie spełnia f ==> (filter f? '(jakiś-lista))

;;;;;;;;;;;;Zadanie9;;;;;;;;;;;;;

(define (append1 xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append1 (cdr xs) ys))))

(foldl append '()  '( (1 2) (2 3) (3 4)))

;
;Foldl bierze 3 argumenty: operacje, wartosc neutralna (chyba to sie tak nazywa) i liste
;I wykonuje po kolei dana operacje na tej liscie
