#lang racket

;; arithmetic expressions

(define (const? t)
  (number? t))

(define (op? t);;oprracja?
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e);;znak
  (car e))

(define (op-args e);;argumenty operacji
  (cdr e))

(define (op-cons op args);;twożenie operacji
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

;; lets

(define (let-def? t);;jak wygląda let
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e);;cząść, gdzie przypisujeszwartości
  (car e))

(define (let-def-expr e);;wyrażenie
  (cadr e))

(define (let-def-cons x e);;twożenie let
  (list x e))

(define (let? t);;otagowany let
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e);;twożenie otagowanego
  (list 'let def e))

;; variables-ZMIENNE

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

;; pairs

(define (cons? t);;lista 2 liczb/znaków otagowana 'cons
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'cons)))

(define (cons-fst e);;I-szy el pary
  (second e))

(define (cons-snd e);;II-gi el pary
  (third e))

(define (cons-cons e1 e2);;twożenie pary
  (list 'cons e1 e2))

(define (car? t);;otagowana 'car lista ('car symbol)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'car)))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'cdr)))

(define (cdr-expr e)
  (second e))

;; lambdas

(define (lambda? t);;('lambda (lista symboli) (wyrażenie))
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'lambda)
       (list? (cadr t))
       (andmap symbol? (cadr t))))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; applications-CZY DZIAŁANIE????????-do lambdy?

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

(define (app-proc e)
  (car e))

(define (app-args e)
  (cdr e))

;; expressions

(define (expr? t);;stałą/ var-zmienna /operacja binarna/let-wyrażenie/cons/car/cdr/lambda/app
  (or (const? t)
      (and (op? t)
           (andmap expr? (op-args t)))
      (and (let? t)
           (expr? (let-expr t))
           (expr? (let-def-expr (let-def t))))
      (var? t)
      (and (cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))))

;; environments-ŚRODOWISKO

(define empty-env;;puste
  null)

(define (add-to-env x v env);;dodawanie zmienna, wartość do środowiska 
  (cons (list x v) env))

(define (find-in-env x env);;szukanie wartości zmiennej x w env-jak jest, to zwróci wartość, jak nie to error
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; closures-domknięcia 
;(eval '(let (x 5) (lambda (z) (let (y 5) (+ x y z))))) ==> '(closure (z) (let (y 5) (+ x y z)) ((x 5)))
;                                                           ( clousure (lista zmiennych) (wyrażenie) (środowisko= (lista par (zmienna wartość))))

(define (closure-cons xs expr env);twożenie domknięcia 
  (list 'closure xs expr env))

(define (closure? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'closure)))

(define (closure-vars c)
  (cadr c))

(define (closure-expr c)
  (caddr c))

(define (closure-env c)
  (cadddr c))

;; evaluator

(define (eval-env e env);;poaje wyrażenie i środowisko, a on daje domknięcie
  (cond [(const? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]
        [(cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(app? e)
         (apply-closure
           (eval-env (app-proc e) env)
           (map (lambda (a) (eval-env a env))
                (app-args e)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply-closure c args)
  (eval-env (closure-expr c)
            (env-for-closure
              (closure-vars c)
              args
              (closure-env c))))

(define (env-for-closure xs vs env);;środowisko dla domknięcia ?
  (cond [(and (null? xs) (null? vs)) env]
        [(and (not (null? xs)) (not (null? vs)))
         (add-to-env
           (car xs)
           (car vs)
           (env-for-closure (cdr xs) (cdr vs) env))]
        [else (error "arity mismatch")]))

(define (env-for-let def env);;twoży środowisko dla let
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))


