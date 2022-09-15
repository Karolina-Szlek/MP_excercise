#lang racket

;============
;Zadanie 1
;============

#|---------------------------------
Składniowe wiązanie zmiennych
------------------------------------|#

(define (f)
  (let ([y 10]);tu "ważniejsze" przypisanie y wartości
    (lambda (x) (+ x y))))

(define y 0)

((f) 5) ;==>Zwróci 15, ponieważ y=10, a x=5
            ;y=10, ponieważ wiązanie zmiennej wewnątrz defina ma "pierszeństwo"
            ;nad globalnymi (poza define funkcji)w składniowym wiązaniu zmiennych

#|---------------------------------
Dynamiczne wiązanie zmiennych
------------------------------------|#
;;twoży się jedna wielka tablica zmiennych (zdefiniowane globalnie)

(define (f1)
  (let ([y1 10]);; nie ma znaczenia, ponieważ |
    (lambda (x) (+ x y1))));                  |
;                                            V
(define y1 0);;globalne przypisanie wartości jest tu "ważniejsze" niż lokalne, więc y=0 

((f1) 5); ==> zwróci 5, ponieważ y=0 i x=5
            ; globalne (poza funkcją) wiązanie zmiennych ma "pierwszeństwo" przed
            ; tym wewnętrznym (lokalnym ?)


;=============
;Zadanie 2
;=============
#|
na kodzie z wykładu (wykład1.rkt) puszczasz np.(eval '(let (x 5) (lambda (x) (let (y 5) (+ x y)))))
i zwraca ci domknięcie

A) (let (x 5) (lambda (z) (let (y 5) (+ x y z))))==> '(closure (z) (let (y 5) (+ x y z)) ((x 5)))
B) (let (x 5) (lambda (x) (let (y 5) (+ x y))))==> '(closure (x) (let (y 5) (+ x y)) ((x 5)))
C) ((lambda (x) (lambda (y) (+ x y))) 10)==> '(closure (y) (+ x y) ((x 10)))

przy wiązaniu dynamicznym nie zapamiętuje się śrosowiska 
|#

















