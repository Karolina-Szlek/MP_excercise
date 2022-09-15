#lang racket

;LAMBDAAAAAA λ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;---------;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ~~λ~~~λ~~ Z POZDROWIENIAMI DLA PAROSTATKU ~~λ~~~λ~~


;twoerdzenie o indukcji dla drzew
;1) p(lisc) zachodzi
;2) dla dowolnego e,l,r
;jezeli p(l) i p(r) to p((node el r)) zachodzi rowniez
;wynik) to dla dowolnego t, jesli (tree? t), to p(t) zachodzi
#|
(define (mirror t)
  (if (leaf? t)
      leaf
      (node (node-elem t)
            (mirror (node-right t))
            (mirror (node-left t)))))

--------------------------
λ = rownowazne
--------------------------

tw
jesli (ttree? t) to (mirror (mirror t)) λ t

dowod
przez indukcje

1) T (mirror (mirror leaf)) λ leaf

(mirror leaf) λind leaf (okii)

2) wezmy e l r t.że (mirror (mirrorl)) λ l
     i
   (mirror(mirror r)) λ r

;-----------------------

T:  (mirror (mirror( node e l r))) λ (node e l r)
z definicji
(mirror (node e (mirror r) (mirror r)))
z zalozenia
(node e (mirror ( mirror l))  (mirror (mirror l)))
z zalozenia to jest λ
(node e l r) (okii)
λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ


(define (flat-app t xs)
  (if (leaf? t) xs
      (flat-app (node-left t)
                (cons (node-elem t)
                      (node-right t)
                       xs)))))

(define (flatten t)
   (if (leaf? t) null
       (append (flatten  (node-left t))  (cnos (node-elem t) (flatten (node-right t)))))))


TW:
 Jesli (tree? t) to (flatten t) λ (flat-app t r l)

Dowod przez indukcje :P
hihihihihihihihih
λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ

LEAMAT:

Jesli (tree? t) to dla dowolnego xs (flat-app t xs) λ (append (flatten t) xs)

DOWOD LEMATU:

Przez indukcje wzgledem t

1) T: (flat-app laef xs) λ  (apppend (flatten leaf) xs)
                    z definicji
            xs           λ   (append null xs)

2) wezmy dowolne e r l t.ze
    a) dla dowolnego xs (flat-app l xs) λ (append flatten l) xs)
    b) dla dowolnego xs (flat-app r xs) λ (append (flatten r) xs)
T: dla dowolnego xs, (flat-app (node e l r) xs) λ (append (flatten (node e l r )) xs)
                               z definicji 
   (flat-app l (cons e (flat-app r xs))) ||  (append (append (flatten l) (cons e (flatten r))) xs)
          z zalozenia a)                               łączność appenda
   (append (flatten l) (cons e (flat-app r xs))) || (append (flatten l) (append (cons e (flatten r) xs))
          z zalozenia b)                                    z definicji
   (append (flatten l) (cons e (append (flatten r) xs))) || (append (cons e (flatten r) xs))

λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ
                                     DRUGA CZESC WYKLADU
λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ

>(eq? 'a 'a)
#t

>(eq? (cons 1 2) (cons 1 2))
#f

>(let ((a (cons 1 2))
     (eq? a a))
#t

--------------------definicja appenda :D------------------------

(define (append xs ys)
   (if (nul? xs) ys
       (cons (car xs) (append (cdr xs) ys)))

----------------------------------------------------------------
         2
        / \
       1   3
      / \ / \

----------------------------------------------------------------

(define (app-iter xs ys)
   (let ((rxs (rev xs)))
      (rev append( rxs ys)))


POSTAC NA UKLADZIE KARTEZJANSKIM (x i y to pary)
(define (make-rect x y)
   (list 'rect x y))

POSTAC BIEGUNOWA
(define (make-circ l alfa)
   (list 'circ l alfa))

(define (vect? x)
   (or (rect? x)
       (circ? x)))

(define (circ->rect c) (...))
(define (rect->circ c) (...))

(define (vec-add v1 v2)
    (let ( (r1 (if (rect? v1) v1 (circ->rect v1)))
           (r2--||-- )
        (make-rect... reszta na fotce wyslanej jako dodatek :3)

Lovki Misiaki <3

|#

