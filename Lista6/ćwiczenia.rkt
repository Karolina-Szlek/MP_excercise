#lang racket


;; arithmetic expressions

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

;; calculator

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-arith e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-arith (binop-left  e))
            (eval-arith (binop-right e)))]))

;; let expressions

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-defs? t)
  (and (list? t) (andmap let-def? t)))

(define (let-defs e) (cadr e))
(define (let-def e) (car e))

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
       (let-defs? (cadr t))))

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
      (and (binop? t)
           (arith/let-expr? (binop-left  t))
           (arith/let-expr? (binop-right t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-defs (let-def-expr t))))
      (and (if-zero? t)
           (arith/let-expr? (if-zero-if t))
           (arith/let-expr? (if-zero-true t))
           (arith/let-expr? (if-zero-false t)))
      (var? t)))

;; evalation via substitution

(define (subst e x f)
  (cond [(const? e) e]
        [(binop? e)
         (binop-cons
           (binop-op e)
           (subst (binop-left  e) x f)
           (subst (binop-right e) x f))]
        [(let? e)
         (let-cons
           (let-def-cons
             (let-def-var (let-defs e))
             (subst (let-def-expr (let-defs e)) x f))
           (if (eq? x (let-def-var (let-defs e)))
               (let-expr e)
               (subst (let-expr e) x f)))]
        [(var? e)
         (if (eq? x (var-var e))
             f
             (var-var e))]))

(define (eval-subst e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-subst (binop-left  e))
            (eval-subst (binop-right e)))]
        [(let? e)
         (eval-subst
           (subst
             (let-expr e)
             (let-def-var (let-defs e))
             (eval-subst (let-def-expr (let-defs e)))))]
        [(var? e)
         (error "undefined variable" (var-var e))]))

;; evaluation via environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (if-zero-if e) (second e))
(define (if-zero-true e) (third e))
(define (if-zero-false e) (fourth e))

(define (if-zero e)
  (if (= (if-zero-if e) 0)
      (if-zero-true e)
      (if-zero-false e)))
#|
(define (if-zero? e)
  (and (list? e)
       (= 4 (length e))
       (eq? (car e) 'if-zero)))


(define (eval-env e env)
  (cond [(if-zero? e)
         (if(= (eval-env (if-zero-if e) env) 0)
            (eval-env (if-zero-true e) env)
            (eval-env (if-zero-false e) env))]                 
    [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(if-zero? e) (if (= 0 (eval-env (if-zero-if e) env))
                          (eval-env (if-zero-true e) env)
                          (eval-env (if-zero-false e) env))]
 ;       [(oper? e) (apply (op->proc (oper-op e)) (map (lambda (a) (eval-env a env)) (oper-arg e)))]
        [(let? e)
         (eval-env
           (let-expr e)
           (eval-let (let-defs e) env))]
        [(var? e) (find-in-env (var-var e) env)]))|#
(define (eval e)
  (eval-env e empty-env))

(define (if-zero? e)
  (and (list? e)
       (eq? (first e) 'if-zero)))

(define (eval-env e env)
  (cond [(if-zero? e)
         (if (= (eval-env (second e) env) 0)
             (eval-env (third e) env)
             (eval-env (fourth e) env))]
        [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(let? e)
         (eval-env
           (let-expr e)
           (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]))

(eval '(if-zero  (- 2 3) (/ 1 2) 9))

(define (eval-let dfs env)
  (define (f def gl-env)
    (if (null? def)
        gl-env
        (f (cdr def) (add-to-env (let-def-var (let-def def))
                                 (eval-env (let-def-expr (let-def def)) env)
                                           gl-env))))
  (append (f dfs empty-env) env))
(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Zadanie 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xs lista liczb
; ys lista znaków
;exp lista z zapisanum wytażeniem

;symbol musi być zacytowany
(define (smbol? e)
  (or (eq? e +)
      (eq? e -)
      (eq? e *)
      (eq? e /)))

;(symbol? '(2 +))
;(symbol? '*)
;(symbol? 3)

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))  

;(define (flatten l)
;  (cond
;    [(empty? l) '()]
;    [(not (list? l)) (list l)]
 ;   [else (append (flatten (first l)) (flatten (rest l)))]))

;(define (flatten t)
;  (define (help t acc)  ;;help dodaje tę wartość na początek akulumatora
;    (if (leaf? t)
;        acc 
 ;       (help (node-left t) (cons (node-val t) (help (node-right t) acc)))))
 ; (help t '()))


;;bierze początkowe 2 liczby i ostatni operator

;;bez flattena
(define (arith-to-rpn e)
  (define (helper e acc)
    (cond [(const? e) (cons e acc)]
          [(binop? e) (helper (binop-left e) (helper (binop-right e) (cons (binop-op e) acc)))]
          [else (error " ")]))
  (helper e null))

(arith-to-rpn '(+ 2 (* 2 3)))
;(arith-to-rpn '(+ 6 (+ 2 (* 2 3))))


;; flatten

(define (airth->rpn xs ys exp)
  (if (null? exp)
      (flatten (append (flatten xs) (flatten ys)))
      (cond
    [(symbol? (car exp)) (airth->rpn xs (list (car exp) ys) (cdr exp))]
    [(number? (car exp)) (airth->rpn (list xs (car exp)) ys (cdr exp))])))


(define (rpn e)
  (define (ar->rpn exp)
    (cond [(const? exp) (list exp)]
          [(binop? exp) (list (ar->rpn (binop-left exp))
                              (ar->rpn (binop-right exp))
                              (binop-op exp))]))
    (flatten (ar->rpn e)))

;(airth->rpn '() '() '(2 + 2 * 2))
;(rpn '(+ 2 (* 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Zadanie 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;stos-lista początek, to wierzchołek stosu
; pop- zwraca listę 2 list pierwsza z elem usuniętym , druga z resztą stosu

(define (empty? stack)
  (null? stack))

(define empty-stack null)

(define (stack1? s)
  (and (eq? (car s) 'stack)
       (list? s)))   

(define (stack? s)
  (or (null? s)
      (and (const? (car s))
           (stack? (cdr s)))))

(define (push e stack)
  (cons stack e))
                                     
(define (pop s)
  (cons (cdr s) (car s)))


;(stack? '( 6 3 3 1 33 4))

;(push 4 '(6 3 3 1 33 4))

(pop '(6 3 7 3 1 33 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Zadanie3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (oper? op)
  (cond [(eq? op '+) #t]
        [(eq? op '*) #t]
        [(eq? op '-) #t]
        [(eq? op '/) #t]
        [else #f]))
;(op? '+)


;jak znak, to skaracam exp , a zze stosu zabieram 2 górne elementy , robie na nich działanie ze znaku
;, stos skracam o te 2 elementy i na skrócony dodaje wynik działania

(define (eval-rpn exp)
  (define (helper exp stval)
    (cond
      [(null? exp) (car (pop stval))]
      [(number? (car exp)) (helper (cdr exp) (push (car exp) stval))]
      [(oper? (car exp))
       (let ((x (car (pop stval)))
         (y (car (pop (cdr (pop stval))))))
         (helper (cdr exp) (push ((op->proc (car exp)) y x)
                 (cdr (pop (cdr (pop stval)))))))]))
       (helper exp empty-stack))
#|
(define (eval-rpn expr)
  (define (help expr stack)
    (cond [(null? expr) (cdr stack)]
          [(number? (car expr))
           (help (cdr expr) (push (car expr) stack))]
          [(oper? (car expr))
           (let*
               ((fst (car (pop stack)))
                (scd (car (pop (cdr (pop stack)))))
                (elem ((op->proc (car expr)) scd fst)))
             (help (cdr expr) (push elem (cdr (pop (cdr (pop stack)))))))]))
  (help expr null))
|#

;(eval-rpn '(2 2 3 * +))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Zadanie 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;
  














                  













