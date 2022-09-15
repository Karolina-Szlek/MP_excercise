#lang racket

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

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))



(define (hole-context e)
  (define (helper e acc)
    (cond [(hole? e) acc]    
          [(let? e) (cond [(arith/let/hole-expr? (let-def-expr (let-def e))) (helper (let-def-expr (let-def e)) acc)]
                          [else (helper (let-expr e)
                                        (cond [(not (not (member (let-def-var (let-def e)) acc))) acc]
                                            [else (cons (let-def-var (let-def e)) acc)]))])]  
          [(and (binop? e) (arith/let/hole-expr? (binop-right e))) (helper (binop-right e) acc)]                   
          [else (helper (binop-left e) acc)]))
  (helper e '()))



;;t -wyrażenie, na któryn zrobimy hole-context
;; correct -sopdziewany prawidłowy wynik, dla (hole-context t)
;; w ostatnim sprawdzaniu podałam nie prawdziwy oczekiwany wynik więc (test) powinien dla ostatniego zwrócić #f, a dla reszty #t
(define (test)
  (define (check-it t correct) 
    (and
         (null? (remq* (hole-context t) correct))
         (null? (remq* correct (hole-context t)))))
    (values
     (check-it '(* 9 hole) '())
     (check-it '(+ hole 7) '())
     (check-it '(let (a (let (b 1) (/ hole b))) (+ 0 a)) '(b))
     (check-it '(+ (let(x 4) 5) hole) '())
     (check-it '(let(a 12) (let (b 1) (+ b hole))) '(a b))
     (check-it '(let(piesek 1) (let(kotek 7) (let(piesek 9)(let(chomik 5) hole)))) '(chomik kotek piesek))
     (check-it '(let(piesek 1) (let(kotek 7) (let(piesek 9)(let(chomik 5) hole)))) '(chomik lew piesek))))



(test)
