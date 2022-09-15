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

;
(define (let-lift e)
  (define (name-again e iter env) ;;zwróci ((przerobione wyrażenie ) ilość zmiennch)
    (define (r-in-op env iter args)
      (cond [(null? args) (list '() iter)]
            [else (let* ([start (name-again (car args) iter env)]
                         [n_iter (second start)]
                         [arg (first start)]
                         [next (r-in-op env n_iter (cdr args))])
        (list (cons arg (first next)) (second next)))]))
    (cond [(var? e) (list (find-in-env e env) iter)]
          [(const? e) (list e iter)]
          [(op? e)  (list (op-cons (op-op e) (first (r-in-op env iter (op-args e))))
                             (second (r-in-op env iter (op-args e))))]
          [(let? e)
           (let* ([start (name-again (let-def-expr (let-def e)) iter env)]
                  [n-iter (second start)]
                [def-var-n (number->symbol n-iter)]
                [n-env (add-to-env (let-def-var (let-def e)) def-var-n env)]          
                [stop (name-again (let-expr e) (+ 1 n-iter) n-env)])
             (list (let-cons (let-def-cons def-var-n (first start))
                             (first stop))
                 (second stop)))]))
  (define (decompose e env);;(wyrażenie (lista definicji z letów-środowisko))
    (define (d-op args env)
      (cond [(null? args) (list '() env)]
            [else (let*
                      ((start (decompose (car args) env))
                     (el (first start))
                     (n-env  (second start))
                     (stop (d-op (cdr args) n-env)))
                    (list (cons el (first stop)) (second stop)))]))
  (cond [(const? e) (list e env)]
        [(var? e) (list e env)]
        [(op? e) (list (op-cons (op-op e) (first (d-op (op-args e) env)))  (second (d-op (op-args e) env)))]
          [(let? e) (let* ((n-def-expr (first (decompose (let-def-expr (let-def e)) env)))
                         (n-env (second (decompose (let-def-expr (let-def e)) env))))
                    (list (first (decompose (let-expr e) n-env))
                          (add-to-env (let-def-var (let-def e)) n-def-expr (second (decompose (let-expr e) n-env)))))]))
  (define (s-env env);;posortuje (listę def z letóe-środowisko)
    (sort env #:key car (lambda (x y) (symbol<? x y))))
  (define (kaskada env e);;połączy w ostateczne wyrażenie
    (cond
      [(null? env) e]
      [else (let-cons (car env) (kaskada (cdr env) e))]))
  (let* ([renamed-e (car (name-again e 0 '()))]
         [decomposed-e (decompose renamed-e '())] 
         [sorted-env (s-env (second decomposed-e))])
    (kaskada sorted-env (first decomposed-e))))


;;Testy
                         
(display "Test 1")  
(newline)
(let-lift '(+ (let (a 2) (+ 51 a)) 11))
(let-lifted-expr? (let-lift '(+ (let (a 2) (+ 51 a)) 11)))
(newline)


(display "Test 2")  
(newline)
(let-lift '(* 3 (let (a 1) (+ 1 a))))
(let-lifted-expr? (let-lift '(* 3 (let (a 1) (+ 1 a)))))
(newline)

(display "Test 3")  
(newline)
(let-lift '(+ (let (x 3) (+ x (let (y 4) (+ 2 y)))) (let (b 4) (+ 6 b))))
(newline)
(let-lifted-expr? (let-lift '(+ (let (x 3) (+ x (let (y 4) (+ 2 y)))) (let (b 4) (+ 6 b)))))
(newline)

(display "Test 4")  
(newline)
(let-lift '(+ (let (x 3) (* x 13)) (let (z 0) (+ z (let (z 8) (+ 9 z))))))
(let-lifted-expr? (let-lift '(+ (let (x 3) (* x 13)) (let (z 0) (+ z (let (z 8) (+ 9 z)))))))
(newline)

(display "Test 5")
(newline)
(let-lift '(+ (- (let (x (let (x 9) (+ 2 x))) (+ x 9)) (let (x 3) (+ x (let (x 4) (+ 32 (let (x 9) (* 8 x)))))))
                            (+ (let (x 7) (+ (let (x 41) (- x 2)) x)) (let (x 3) (+ (* x 2) 2)))))
(let-lifted-expr? (let-lift '(+ (- (let (x (let (x 9) (+ 2 x))) (+ x 9)) (let (x 3) (+ x (let (x 4) (+ 32 (let (x 9) (* 8 x)))))))
                            (+ (let (x 7) (+ (let (x 41) (- x 2)) x)) (let (x 3) (+ (* x 2) 2))))))




;;Brałam udział w zajęciach w KSI w niedzielę wieczorem organizowanych przez Mateusza Kacałę