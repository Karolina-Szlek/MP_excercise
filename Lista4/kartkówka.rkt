#lang racket

(define (leaf? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'leaf)))

(define (leaf v) (list 'leaf v))

(define (node-left x)
  (cadr x))

(define (node-right x)
  (caddr x))

(define (make-tree node-left node-right)
  (list 'node node-left node-right))

;(define (node? t)
;  (or (leaf? t)
;      (and (list? t)
;           (= 3 (length t))
;           (node? (node-right t))
;           (node?( node-left t)))))


(define (count-zeros t)
    (cond ((and (leaf? t) (= 0 (car (cdr t)))) 1)
          ((and (leaf? t) (not (= 0 (car (cdr t))))) 0)
          (else (+ (count-zeros (node-left t))
                   (count-zeros (node-right t))))))



;;Przykładowe testy dla drzew z różną ilością zer 
(define drzewo (make-tree (make-tree (leaf 5) (leaf 0)) (make-tree (leaf 6) (leaf 3))))
(define drzewo1 (make-tree (make-tree (leaf 5) (leaf 0)) (make-tree (leaf 6) (leaf 0))))
(define drzewo2 (make-tree (make-tree (leaf 5) (leaf 1)) (make-tree (leaf 6) (leaf 2))))
(count-zeros drzewo)
(count-zeros drzewo1)
(count-zeros drzewo2)

;;Karolina Szlęk




(define (filt p xs)
  (if (null? xs)
      null
      (if (eq? #t (p (car xs)))
          (append (list (car xs)) (filt p (cdr xs)))
          (filt p (cdr xs)))))