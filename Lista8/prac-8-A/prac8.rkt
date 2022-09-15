#lang racket
#|
;;;;;;;;;;;;Pracownia 8 A
warto dodać listy do naszego bazowego ewaluatora, aby łatwiej pisać testy, a nie jakieś cons cons :D 
pair -> list
czyli chcemy umieć przeczytać ('list ('quote ...
ewaulator zawsze działa rekurencynie, czyli wykorzystamy lambda-rec
|#


'(let (x (lambda-rec (eval-new expr) ;; 1) nazwa 2) zmienne
                     (cond
                       [(pair? expr)  ;;na początku sprawdzamy czy coś jest listą (a tak na prawdę parą), bo dostajemy wyrażenie lispowe, 
                                (cond
                                     [(eq? (car expr) ;;sprawdzamy czy jest znakiem
                                                 (quote +)) ;;quote + to jest symbol w bazowym języku 
                                                           (+ (eval-new (drugi element expr)) (eval-new (trzeci element expr)))]
                                                                     
                                    ;; [ -.- (quote -) (quote *) (quote /)])]))) ;;to samo dla innych znaków,
                                   )]   
                       [true e])))) ;;zamiast else, bo go nie mamy w tym języku, w tym miejscu zamykamy leta
 ;;(x (list (quote +) 2 3)) ;; to zwróci 5  - to jest test 

;;(cons (quote +) (cons 1 (cons 2 my-null)))
#|
podsumowanie
1) cukier synktaktyczny
2) napisanie rekurencyjnej lambdy w naszym języku, 



;;;Zadanie B wariant nr 3

1)zmieniamy nazwy zmiennych
2) bierzemy algorytm dodajemy nowe kosktruktory null? coś tam jeszcze lambda, lambda-rec, wszystko to co od ostatniego czasu dodaliśmy do ewaluatora



|#


