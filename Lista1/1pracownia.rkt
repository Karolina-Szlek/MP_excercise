#lang racket


(define (cube-root x)
  (define (improve approx)
    (/ (+ (/ x (* approx approx)) (* 2 approx)) 3))
  (define (dist x y)
  (abs (- x y)))
  (define (cube x)
  (* x x x))
  (define (good-enough? approx)
    (< (dist x (cube approx)) 0.0001))
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
  (iter 1.0))


(cube-root 8)
(cube-root 0)
(cube-root -27)
(cube-root 5)
(cube-root 125)