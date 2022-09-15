#lang racket

;; expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t);;czy poprawne wyrażenie arytmet
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t);;czy to co zrobiliśmy jest w dobrej formie 
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i);;twożey dla i= 2 x2 itp.
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; the let-lift procedure

;lista 2 list[(() () ()) (wyrażenie)]
(define (let-lift e)'())


(define (rename expr iter env)
  (define (r-in-op env iter args) ;;
  (cond [(null? args) (list null iter)]
        [else (let* ([first-renamed (rename (car args) iter env)]
                     [n_iter (second first-renamed)]
                     [elem (first first-renamed)]
                     [tmp2 (r-in-op env n_iter (cdr args))]
                     [res (first tmp2)]
                     [last-iter (second tmp2)])
        (list (cons elem res) last-iter))]))
  (cond [(var? expr) (list (find-in-env expr env) iter)]
        [(const? expr) (list expr iter)]
        [(op? expr)
         (let* ([tmp (r-in-op env iter (op-args expr))]
                [args (first tmp)]
                [n_iter (second tmp)])
           (list (op-cons (op-op expr) args) n_iter))]
        [(let? expr)
         (let* ([tmp (rename (let-def-expr (let-def expr)) iter env)]
                [new-let-def-expr (first tmp)]
                [new-iter (second tmp)]
                [new-def-var (number->symbol new-iter)]
                [new-env (add-to-env (let-def-var (let-def expr)) new-def-var env)]          
                [tmp2 (rename (let-expr expr) (+ 1 new-iter) new-env)]
                [new-let-expr (first tmp2)]
                [latest-iter (second tmp2)])
           (list (let-cons (let-def-cons new-def-var new-let-def-expr)
                           new-let-expr)
                 latest-iter))]))




(define (decompose e env)
  (define (d-op args env)
  (cond [(null? args) (list null env)]
        [else (let* ((tmp (decompose (car args) env))
                     (el (first tmp))
                     (n-env  (second tmp))
                     (tmp2 (d-op (cdr args) n-env))
                     (res (first tmp2))
                     (nn-env (second tmp2)))
                (list (cons el res) nn-env))]))
  (cond [(const? e) (list e env)]
        [(var? e) (list e env)]
        [(op? e) (let* ((tmp (d-op (op-args e) env))
                        (n-args (first tmp))
                        (n-env (second tmp)))
                        (list (op-cons (op-op e) n-args) n-env))]
        [(let? e) (let* ((tmp (decompose (let-def-expr (let-def e)) env))
                         (n-def-expr (first tmp))
                         (n-env (second tmp))
                         (var (let-def-var (let-def e)))
                         (fin (add-to-env var n-def-expr (second (decompose (let-expr e) n-env)))))
                    (list (first (decompose (let-expr e) n-env)) fin))]))
                         
                         
  

(define (s-env env)
  (sort env #:key car (lambda (x y) (symbol<? x y))))

(define (kaskada env e)
  (cond
    [(null? env) e]
    [else (let-cons (car env) (kaskada (cdr env) e))])) 




(define a (car (rename '(+ 9 (let (x 2) (+ 1 x))) 0 '())))

(kaskada (s-env (second (decompose a '()))) (car (decompose a '())))

;(decompose '(+ 9 (let (x 2) (+ 1 x))) '())
;(let-lift '(+ (let (x 3) (+ x (let (y 4) (+ 2 y)))) 2))
;(let-lift '(+ (let (x 3) (+ x (let (y 4) (+ 2 y)))) (let (z 0) (+ z (let (p 8) (+ 9 p))))))                
;(decompose (rename '(+ (+ (let (x 8) (+ x (let (x 9) (+ 2 x)))) (let (x 3) (+ x (let (x 4) (+ 2 x)))))
                          ;  (+ (let (x 3) (+ x (let (x 4) (+ 2 x)))) (let (x 3) (+ x (let (x 4) (+ 2 x)))))) 0 '()) '())
