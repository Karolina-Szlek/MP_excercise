#lang racket

;;;Zadanie 2-----------------------------

(require racket/contract)

(define/contract foo number? 42);=> 42, jak zamiast 42 dam nie liczbe to wyrzuci bład kontraktu

(define/contract (dist x y);=> y-x 
  (-> number? number? number?);sprawdza czy składniki i wynik to liczby??? jak nie to bład contract
  (abs (- x y)))

(define/contract (average x y)
  (-> number? number? number?)
  (/ (+ x y) 2))

(define/contract (square x)
  (-> number? number?)
  (* x x))

(define/contract (sqrt x)
  (->i ([x positive?])
       [result positive?]
       #:post (x result);po wykonaniu ma to sprawdzić -mówie, ż czgo kożystam 
       (if (< (dist x (square result)) 0.0001);; tu piszesz co chcesz, ale wynikiem tefo ma być boolean 
           #t
           #f))
  (define (improve approx)
    (average (/ x approx) approx))
  (define (good-enough? approx)
    (< (dist x (square approx)) 0.0001))
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
(iter 1.0))

(define/contract (sqrt x)
  (->i ([x positive?])
       [result (x) (lambda (res)
                     (and 
                      (< (dist x (square res)) 0.0001)
                          (positive? res)))])
  ;; lokalne definicje
  ;; poprawienie przybliżenia pierwiastka z x
  (define (improve approx)
    (average (/ x approx) approx))
  ;; nazwy predykatów zwyczajowo kończymy znakiem zapytania
  (define (good-enough? approx)
    (< (dist x (square approx)) 0.0001))
  ;; główna procedura znajdująca rozwiązanie
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
  (iter 1.0))

(sqrt 2)



;;Zadanie 1------------------------------------
(define/contract (sufixes xs)
  (let ((a (new-∀/c 'a)))
        (-> (listof a) (listof (listof a))))
  (if (null? xs)
     (list '())
     (cons xs (sufixes (cdr xs))))) 

(sufixes '(1 2 3 4))


;;Zadanie 3------------------------------------

#|
(define/contract (filter1 f xs)
  (let ((a (new-∀/c 'a)))
        (->i ([f (-> a boolean?)]
              [xs (listof a)])
             (result (f)
             (lambda (res) (foldr  and #t (map f res)))))));;zapytaj kto ma Agnieszka ?|#

;;Zadanie 4----------------------------------

(define-signature monoid^
  ((contracted
    [elem? (-> any/c boolean?)]
    [neutral elem?]
    [oper (-> elem? elem? elem?)])))



(define-unit monoid-la@
  (import)
  (export monoid^)

  (define (elem? x)
    (integer? x))
  
  (define neutral 0)
  
  (define (oper x y)
    (+ x y)))



(define-unit monoid@
  (import)
  (export monoid^)
  
  (define (elem? x)
    (list? x))
  
  (define (oper xs ys)
    (append xs ys))
  (define neutral empty))

(define-values/invoke-unit/infer monoid-la@)

 (require quickcheck)


;(quickcheck
 ;(property
  ;((k (aribtrary-intiger))
   ;(l (aribtrary-intiger))
        

;;Zadnaie 6--------------------------
               


(define (prefixes xs)
  (define/contract (help ys)
    (let ((a (new-∀/c 'a))
          (b (new-∀/c 'b)))
      (-> (listof a) (listof (listof a))))
    (if (null? ys)
        (list '())
        (cons (reverse ys) (help (cdr ys)))))
  (help (reverse xs)))

(prefixes '(1 2 3 4))
























