#lang racket

#|

1. P(l), gdzie l to symbol, musi byc prawda.
2. Dla dowolnych x, y takich, że
(prop? x) oraz (prop? y) (P(x) oraz P(y)) => ( P(neg x) oraz P(conj x y) oraz P(disj x y) )
Z tych dwoch warunkow wynika, że dla dowolnego t,
takiego ze (prop? t) P(t) zachodzi.
W tym pierwszym warunku to musi zachodzic (var? l)
 co jest rownowazne temu, ze l jest symbolem

NIEUŻYTEK-no bo jak łączysz dwie listy nim
to ta pierwsza zostaje w pamięci a nie masz do niej dostępu



KONSTRUKTORY vs. SELEKTORY
konstruktory zaczynają się od make-*
a selektory po prostu zwracają jakiś element listy/pary


|#

;;;;;;;;;;;;;;;;;
;Zad 1
;;;;;;;;;;;;;;;;;


(define (var? t);;czy zmienna
  (symbol? t))
 
(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))
 
(define (conj? t);;koniungcja
  (and (list? t)
        (= 3 (length t))
        (eq? 'conj (car t))))
 
(define (disj? t);;alternatywa
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))
 
(define (prop? f);;czy formuła ?
  (or (var? f)
      (and (neg? f)
            (prop? (neg-subf f)))
      (and (disj? f)
            (prop? (disj-left f))
            (prop? (disj-rght f)))
      (and (conj? f)
            (prop? (conj-left f))
            (prop? (conj-rght f)))))

(define (make-neg t)
  (list 'neg t))

(define (neg-subf f);;zanefowana formuła, biore formułe, bez znaku negacji 
  (cadr f)) ;;cadr=second

(define (make-conj l p)
  (list 'conj l p))

(define (make-disj l p)
   (list 'disj l p))

(define (disj-left f)
  (second f))
 
(define (disj-rght f)
  (third f))
 
(define (conj-left f)
  (second f))
 
(define (conj-rght f)
  (third f))


;;;;;;;;;;;;;;;;;;;;;
;Zad 2
;;;;;;;;;;;;;;;;;;;;;


;;free-vars
 
(define (free-vars f)
  (define (iter f xs)
    (cond [(neg? f) (iter (neg-subf f) xs)]
          [(disj? f) (iter (disj-rght f) (iter (disj-left f) xs))]
          [(conj? f) (iter (conj-rght f) (iter (conj-left f) xs))]
          [else (remove-duplicates (append xs (list f)))]))
  (if (prop? f)
      (iter f null)
      (error "To nie jest formuła, z jakiegoś tam powodu")))
 

;(newline)
;(free-vars (make-neg (make-disj 'bala (make-conj 'bala (make-disj 'ee 'ee)))))
;(free-vars (make-neg (make-disj 'bala (make-conj 'bala (make-disj 'ee 123)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;Zad 3
;;;;;;;;;;;;;;;;;;;;;;;;


(define (gen-vals xs)
  (if (null? xs )
      (list null )
      (let*
          ((vss (gen-vals (cdr xs )))
           (x (car xs ))
           (vst (map (lambda (vs ) (cons (list x true ) vs )) vss ))
           (vsf (map (lambda (vs ) (cons (list x false ) vs )) vss )))
        (append vst vsf ))))

;(define (get-value s eval)
;  (if (eq? (car(car eval)) s)
;      (cdr (car eval))
;      (get-value s (cdr eval))))

  (define (get-value s eval)
    (define l (filter
        (lambda (x)
          (if (equal? (car x) s)
              #t
              #f))
        eval))
    (if (equal? l null)
        l
        (cadar l)))

(define (sigma formula wartosciowanie)
  (cond [(var? formula) (get-value formula wartosciowanie)]
        [(disj? formula) (or (sigma (disj-rght formula) wartosciowanie) (sigma (disj-left formula) wartosciowanie))]
        [(conj? formula) (and (sigma (conj-rght formula) wartosciowanie) (sigma (conj-left formula) wartosciowanie))]
        [(neg? formula) (if (eq? (sigma (neg-subf formula) wartosciowanie) #t)
                            #f
                            #t)]))

;(define f (make-conj (make-disj 'p 'q) (make-neg 'r)))
;(sigma f '( (p #t) (q #t) (r #t)))
;(sigma f '( (p #t) (q #t) (r #f)))
;(sigma f '( (p #f) (q #f) (r #t)))


;(car (get-value 'p  '((p #t) (q #f) (r #f))))

(define (eval-formula f wart)
  (filter (lambda (x) (not (sigma f x))) wart))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Zad 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nnf? f);;;czy formuła w negacyjnej formule normalnej ?
  (or (var? f)
      (and (neg? f)
           (var? (neg-subf f)))
      (and (disj? f)
            (nnf? (disj-left f))
            (nnf? (disj-rght f)))
      (and (conj? f)
            (nnf? (conj-left f))
            (nnf? (conj-rght f)))))



(define f2 (make-disj (make-conj 'p 'r) (make-disj (make-neg 's) 'u)))
(nnf? f2)
(define f3 (make-disj (make-neg (make-conj 'p 'r)) (make-disj (make-neg 's) 'u)))
(nnf? f3)
(define f1 (make-neg (make-disj (make-conj 'p 'r) (make-disj (make-neg 's) 'u))))
(nnf? f1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Zad 5       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; wzajemnie rekurencyjna - wywołują siebie nawzajem 


(define (make-nnf formula)
  (cond [(var? formula) formula]
        [(conj? formula) (make-conj (make-nnf (conj-left formula)) (make-nnf (conj-rght formula)))]
        [(disj? formula) (make-disj (make-nnf (disj-left formula)) (make-nnf (disj-rght formula)))]
        [(neg? formula) (pomoc (neg-subf formula))])) 



(define (pomoc f)
  (cond [(var? f) (make-neg f)]
        [(neg? f) (make-nnf (neg-subf f))]
        [(conj? f) (make-disj (pomoc (conj-left f)) (pomoc (conj-rght f)))]
        [(disj? f) (make-conj (pomoc (disj-left f)) (pomoc (disj-rght f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Zad 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







