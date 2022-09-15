#lang racket

;;na drzewach

(define (lcons x f);;para element 1-szy i funkcja
  (cons x f))

(define (lhead l)
  (car l))

(define (ltail l)
  ((cdr l)));;czemu tu tyle nawiasów?

(define (nats-from m);;daje mu m, a on roli (lcons m (proc dająca kolejne liczby -każda o 1 większa))
  (lcons
   m
   (lambda () (nats-from (+ m 1)))))

(define nats;;nats-from dla 0
  (nats-from 0))

(define (take n l);;daje nam n liczb z listy l ??????
  (if (or (null? l) (= n 0))
      null
      (cons (lhead l)
            (take (- n 1) (ltail l)))))

(define (filter p l)
  (cond [(null? l) null]
        [(p (lhead l))
         (lcons (lhead l)
                (lambda ();;na dalszej części listy zrobiont filter
                  (filter p (ltail l))))]
        [else (filter p (ltail l))]))

(define (prime? n)
  (define (div-by m)
    (cond [(= m n) true]
          [(= (modulo n m) 0) false]
          [else (div-by (+ m 1))]))
  (if (< n 2)
      false
      (div-by 2)))

;;;;;;;;;;;;;;Zadanie 1;;;;;;;;;;;;;

;;definiujesz procedure, od el, który będzie pierwszy
;;i funkcje (lambda), która mówi, jaki wziąć następny


(define (fibs-from m n)
  (lcons m (lambda () (fibs-from n (+ n m)))))

(define fibs
  ;(fibs-from 1 1)
  (fibs-from 0 1))

;(take 10 fibs)


;;;;;;;;;;;;;;;Zadanie 2;;;;;;;;;;;;;;;

(define (ints-from m)
  (lcons m (if (>= m 0)
               (lambda () (ints-from (- (+ m 1))))
               (lambda () (ints-from (- m))))))
;;jak m jest dodatnie, to wypisz ujemną dla m+1
;;jak m jest ujemne, to wypisz dodatnią m

(define ints
  (ints-from 0))

;(take 10 ints)




















  