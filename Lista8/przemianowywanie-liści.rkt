#lang racket
;;Przemianowywanie liśći używając stanu

(define (node? t);;('node coś)
  (and (pair? t)
       (eq? (car t) 'node)))

(define (node-left t) (second t))
(define (node-right t) (third t))

(define (relabel t i);;zamiana t na i
  (if (not (node? t));;jak te nie jest node
      (cons i (+ i 1));;(i (+ i 1))
      (let*
        ([l (relabel (node-left t)
                     i)]
         [r (relabel (node-right t)
                     (cdr l))])
        (cons (list 'node (car l) (car r))
              (cdr l)))));;robie na lewym i prawym poddrzewnie
