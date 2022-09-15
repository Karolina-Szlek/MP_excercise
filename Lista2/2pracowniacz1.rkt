#lang racket

(define (dist x y)
  (abs (- x y)))


(define (nu x ) (* x x))
(define (d x ) (* x x x))

(define (minus x y) (- x y))

(define (con-franc-iter num den k acc)
  (define (half acc k)
    (if (<= k 0)
        acc
        (half (/ (num k) (+(den k) acc)) (- k 1))))
     (half 0 k))


(define (depth num den)
  (define (countA a b n)
    (+ (* (den n) b) (* (num n) a) ))
  
  (define (countB a b n)   
    (+ (* (den n) b) (* (num n) a)))
  
  (define (div a b c d n)
    (/ (countA a b n) (countB c d n)))
  
  (define (close-enough? x y)
    (< (dist x y) 0.01111))
  
  (define (func old a b c d n)
    (if (close-enough? old (div a b c d n)) old
        (func (/ (countA a b n) (countB c d n)) b (countA a b n) d (countB c d n) (+ n 1))))
  
  (func 0 1 0 0 1 1))

(define (checking num den)
  (define (countA a b n)
    (+ (* (den n) b) (* (num n) a) ))
  
  (define (countB a b n)   
    (+ (* (den n) b) (* (num n) a)))
  
  (define (div a b c d n)
    (/ (countA a b n) (countB c d n)))
  
  (define (close-enough? x y)
    (< (dist x y) 0.01111))
  
  (define (func old a b c d n)
    (if (close-enough? old (div a b c d n)) (- n 1)
        (func (/ (countA a b n) (countB c d n)) b (countA a b n) d (countB c d n) (+ n 1))))
  
  (func 0 1 0 0 1 1))


(depth nu d)
(checking nu d);;wyliczam głębokością dla której dostałam wynik
(con-franc-iter nu d 3.0 0)
(con-franc-iter nu d 4.0 0)

;;sprawdź czy naprawdę < 0.01111

(< (minus (con-franc-iter nu d 1.0 0) (con-franc-iter nu d 2.0 0) ) 0.01111);;różnica między wyliczoną głębokością, a poprzednią

(< (minus (con-franc-iter nu d 2.0 0) (con-franc-iter nu d 3.0 0) ) 0.01111);;różnica między wyliczoną głębokością i następną

