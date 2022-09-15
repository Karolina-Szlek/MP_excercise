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

; arith and bool expressions: syntax and semantics

(define (const? t)
  (number? t))

(define (true? t)
  (eq? t 'true))

(define (false? t)
  (eq? t 'false))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= not and or mod rand))))

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
        [(eq? op '<=) <=]
        [(eq? op 'not) not]
        [(eq? op 'and) (lambda x (andmap identity x))]
        [(eq? op 'or) (lambda x (ormap identity x))]
        [(eq? op 'mod) modulo]
        [(eq? op 'exp) expt]))
        ;;[(eq? op 'rand) (lambda (max) (min max 4))])) ; chosen by fair dice roll.
                                                      ; guaranteed to be random.

(define (var? t)
  (symbol? t))

(define (rand? t)
  (tagged-tuple? 'rand 2 t))


(define (eval-arith e m)
  (define (helper m args acc)
    (if (null? args)
        (list acc m)
        (let ((x (eval-arith (car args) m)))
          (helper (second x) (cdr args) (append acc (list (first x)))))))
        ;(helper (second (eval-arith (car args) m)) (cdr args) (append acc (list (first (eval-arith (car args) m)))))))
  (cond [(true? e) (list true m)]
        [(false? e) (list false m)]
        [(var? e) (list (get-mem e m) m)]
        [(rand? e) (let*
                       ([expres (eval-arith (second e) m)]
                        [eval-r ((rand (res-val expres)) (get-mem 'seed (car (res-state expres))))])
                     (list (res-val eval-r) (set-mem 'seed (res-state eval-r) m)))]
        [(op? e) (let* ([done (helper m (op-args e) '())]
                  [e (apply (op->proc (op-op e)) (first done))])
                 (list e (second done)))]
        [(const? e) (list e m)]))

;; syntax of commands

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

(define (block? t)
  (list? t))

;; state

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;; psedo-random generator

(define initial-seed
  123456789)

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

;; WHILE interpreter

(define (old-eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (eval-arith (if-cond e) m)
             (old-eval (if-then e) m)
             (old-eval (if-else e) m))]
        [(while? e)
         (if (eval-arith (while-cond e) m)
             (old-eval e (old-eval (while-expr e) m))
             m)]
        [(block? e)
         (if (null? e)
             m
             (old-eval (cdr e) (old-eval (car e) m)))]))

(define (eval e m seed) ;;;;;;;;;;;;;;;;;;;;;;;;w memory trzymaj seed
  ;; TODO : ZAD B: Zaimplementuj procedurę eval tak, by
  ;;        działała sensownie dla wyrażeń używających
  ;;        konstrukcji "rand".
  (old-eval e (set-mem 'seed seed m)))

(define (run e)
  (eval e empty-mem initial-seed))

;;


(define fermat-test
  '( (composite := false)
     (if (or (= 2 n) (= 1 n))
         ()
         (while (> k 0)
                ( (a := (+ 2 (rand (- n 2))))
                  (if (not (= 1 (mod (expt a (- n 1)) n)))
                  ( (k := 0)
                    (composite := true))
                  (k := (- k 1))))))))
#|(define fermat-test
  '( (composite := false)
     (if (or (= n 1) (= n 2))
         ()
         (while (> k 0)
                ( (a := (+ 2 (rand (- n 2))))
                  (if (= (mod (expt a (- n 1)) n) 1)
                      (k := (- k 1))
                      ( (k := 0)
                        (composite := true)))
                  )
                )
         ))
  )   |#           


(define (probably-prime? n k) ; check if a number n is prime using
                              ; k iterations of Fermat's primality
                              ; test
  (let ([memory (set-mem 'k k
                (set-mem 'n n empty-mem))])
    (not (get-mem
           'composite
           (eval fermat-test memory initial-seed)))))


;(probably-prime? 1 10)
(probably-prime? 2 10)
(probably-prime? 3 10)
(probably-prime? 7 100)
(probably-prime? 12 100)
(probably-prime? 8 100)
;;;poniżej 5 nie działają




              #|(while (< x n)
                     ((new-a := (* new-a a))
                      (x := (+ x 1))))
              (if (not (= 1 (mod new-a n)))
                ( (k := 0)
                  (composite := true) )
                ( (k := (- k 1)) )) ))))|#