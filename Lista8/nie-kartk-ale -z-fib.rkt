#lang racket

;;na drzewach

(define (lcons x f);;para element 1-szy i funkcja
  (cons x f))

(define (lhead l)
  (car l))

(define (ltail l)
  ((cdr l)));;czemu tu tyle nawiasów?-bo funkcja musi buś w nawiasach, żeby byłą wywołana 

(define (nats-from m);;daje mu m, a on roli (lcons m (proc dająca kolejne liczby -każda o 1 większa))
  (lcons
   m
   (lambda () (nats-from (+ m 1)))))

(define nats;;nats-from dla 0
  (nats-from 0))

(define (take n l);;daje nam n liczb z listy l-leniwej ??????
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

;;;;;;;;;;;;;;;NIE KARTKÓWKA;;;;;;;;;;;;;;;;;
(define (fib-from m n)
  (lcons m (lambda () (fib-from n (+ m n)))));m to pierwszt wyraz i rekurencyjne wywołanie w lambdzie ( w środku)

(define fib
  (fib-from 1 1))

(take 10 fib)

(define (even x)
  (lcons x (lambda () (even (+ x 2)))))

(define e
  (even 6))

(take 8 e)

(define (primes x)
  (lcons x (lambda () (if (+
;;;;;;;;;;;;;;;;;;;Wersja II ;;;;;;;;;;;;;;;;;;;

;(define fi-from
 ; (lcons 0 (lambda () (lcons 1 (lambda () (+ map (ltail fib)))))))









