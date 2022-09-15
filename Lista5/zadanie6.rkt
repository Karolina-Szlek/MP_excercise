#lang racket


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

(define (make-neg t)
  (list 'neg t))

(define (neg-subf f);;zanegowana formuła, biore formułe, bez znaku negacji 
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Zad 6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNF będzie reprezentowany w postaci list - (pomysł na taki sposób reprezentacji został przedstawiony na repetytorium).
;; W nawiasach wewnętrznych będzie alteratywa
;; W nawiasach zawnętrznych będzie koniunkcja
;;Przykład ((p s) r) ==> ((p ∨ s) ∧ r)
;;             ^               ^
;;             |               |
;;     to zwróci            tak powinniśmy 
;;    nam make-cnf            to rozumieć



(define (concat-map f x)
  (apply append (map f x)))

(define (literal? l)
  (or (var? l)
      (and (neg? l)
           (var? (neg-subf l)))))

(define (make-cnf formula)
  (cond [(literal? formula) (list (list formula))]
        [(conj? formula) (append (make-cnf (conj-left formula)) (make-cnf (conj-rght formula)))]
        [(disj? formula) (helper (make-cnf (disj-left formula)) (make-cnf (disj-rght formula)))]))

(define (helper  xs ys)
  (concat-map (lambda (x) (map (lambda (y) (append x y)) xs)) ys))


(define (cnf? f)
  (define (good? x)
    (and (list? x)
         (= (length x) (length (filter literal? x)))))
  (and (list? f)
       (or (and (= (length f) 1) (good? (car f)))
           (and (good? (car f)) (cnf? (cdr f))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Testy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "TESTY")
(newline)
(newline)
(define f11 (make-disj (make-conj 'p 'r) (make-disj 's 'u)))
(display f11)
(newline)
(display " Zapis w CNF: ")
(newline)
(make-cnf (make-nnf f11))
(cnf? (make-cnf (make-nnf f11)))
(newline)

(define f1 (make-disj (make-conj 'p 'r) (make-conj 's 'u)))
(display f1)
(newline)
(display " Zapis w CNF: ")
(newline)
(make-cnf (make-nnf f1))
(cnf? (make-cnf (make-nnf f1)))
(newline)

(define f2 (make-conj (make-conj 'p 'r) (make-disj 's 'u)))
(display f2)
(newline)
(display " Zapis w CNF: ")
(newline)
(make-cnf (make-nnf f2))
(cnf? (make-cnf (make-nnf f2)))
(newline)

(define f (make-conj (make-disj 'p 'r) (make-disj (make-neg 's) 'u)))
(display f)
(newline)
(display " Zapis w CNF: ")
(newline)
(make-cnf (make-nnf f))
(cnf? (make-cnf (make-nnf f)))


















  