#lang racket
(define sum
  (lambda (term next s e suma)
    (if (> s e )
        suma
        (sum term next (next s) e (+ suma (term s))))))


(define (p x) (* x x))
(define (inc x) (+ x 1))


(sum p inc 2 7 0)


(define (compose f g )
  (lambda (x) (f (g x))))
(define (identity x ) x)
(define (sq x) (* x x ))

(define (repeated p n )
    ( if(= n 0)
      (identity p)
      (repeated (compose p p) (- n 1))))

((repeated sq 3) 2)


(define (acc comb term next s e null-V)
  (define (acca s acc)
    (if (> s e)
        acc
        (acc (next s) (comb acc (term s)))))
    (acca s null))


(define (con-frac num den k);;liczenie tych śmiesznych ułamków 
  (define (cant1 i k)
    (if (< i k)
        (/ (num i)(+ (den i) (cant1 num den (+ i 1) k )))
        0.0))
  (cant1 num den 0 k))
      


(define (con-franc-iter num den k acc)
  (define (half acc k)
    (if (<= k 0)
        acc
        (half (/ (num k) (+(den k) acc)) (- k 1))))
     (half 0 k))

  
