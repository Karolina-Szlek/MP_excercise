#lang racket


;;remainder - (wbudowany) Returns q with the same sign as n
;;((abs q) is between 0 (inclusive) and (abs m) (exclusive) AND (+ q (* m (quotient n m))), similar to mod


(define (dist x y)
  (abs (- x y)))

(define (square x)
  (* x x))

(define (close-enough? x y)
  (< (dist x y) 0.00001))



(define (fixed-point f x0 err)
  (define (tryIt guess n)
    (if (= 0 n)
        (err "fixed-point didn't converge")
        (let ((next (f guess)))
          (cond
            ((close-enough? guess next) next)
            (else (tryIt next (- n 1)))))))
  (tryIt x0 100))



(define (experiment-root x n levels)
  (define (root-fixp x n)
  (define (exp x n)
  (define (mod? n)
  (= 0 (remainder n 2)))
  (cond [(= 0 n) 1]
        [(mod? n) (exp (square x) (/ n 2))]
        [else (* x (exp x (- n 1)))]))
  (lambda (y) (/ x (exp y (- n 1)))))
  (define (average a b)
  (/ (+ a b) 2.0))
  (define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (if (= 0 n)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))
  (define (average-damp f)
  (lambda (x) (average x (f x))))
  (call-with-current-continuation;;wbudowana (cc) pozwala na zmianę kolejności wykonywania działań
   (lambda (unsuccesful)
     (let ((err (lambda (write) (unsuccesful 'failed))))
       (fixed-point ((repeated average-damp levels) (root-fixp x n)) 1 err)))))


(define (nth-root x n)
  (define (goOn levels)
    (let ((out (experiment-root x n levels)))
      (if (equal? 'failed out)
          (goOn (+ levels 1))
          out)))
  (goOn 1))





;;ile tłumień użyć ? 
(experiment-root 3125 5 4)
(experiment-root 3125 5 5)
(experiment-root 3125 5 6)

(nth-root 27 3)
(nth-root 2 4)
(nth-root 3125 5)
(nth-root 2 2)










