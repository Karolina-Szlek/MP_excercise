#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości
(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; reprezentacja danych wejściowych (z ćwiczeń)//////var=> wartość, zmienna np. p
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

;; przydatne predykaty na zmiennych
(define (var<? x y);;czy y > x
  (symbol<? x y))

(define (var=? x y);; czy równe
  (eq? x y))

(define (literal? x)
  (and (tagged-tuple? 'literal 3 x);;lista ==> ('literal #t / #f <var>)
       (boolean? (cadr x))
       (var? (caddr x))))

(define (literal pol x)
  (list 'literal pol x))

(define (literal-pol x)
  (cadr x))

(define (literal-var x)
  (caddr x))

(define (clause? x);; ('clause <lista var> )
  (and (tagged-list? 'clause x)
       (andmap literal? (cdr x))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c);;zawartość clause 
  (cdr c))

(define (cnf? x);; ('clause <list var>)
  (and (tagged-list? 'cnf x)
       (andmap clause? (cdr x))))

(define (cnf . cs)
    (cons 'cnf cs))

(define (cnf-clauses x);;zawartość cnf
  (cdr x))

;; oblicza wartość formuły w CNF z częściowym wartościowaniem. jeśli zmienna nie jest
;; zwartościowana, literał jest uznawany za fałszywy.
(define (valuate-partial val form)
  (define (val-lit l)
    (let ((r (assoc (literal-var l) val)));;znajduje pare z podaną wartością,potem przypisuje ją r
      (cond
       [(not r)  false];;nie ma pary
       [(cadr r) (literal-pol l)]
       [else     (not (literal-pol l))])))
  (define (val-clause c)
    (ormap val-lit (clause-lits c)))
  (andmap val-clause (cnf-clauses form)))

;; reprezentacja dowodów sprzeczności

(define (axiom? p);;część zb początkowego
  (tagged-tuple? 'axiom 2 p))

(define (proof-axiom c)
  (list 'axiom c))

(define (axiom-clause p)
  (cadr p))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (proof-res x pp pn)
  (list 'resolve x pp pn))

(define (res-var p)
  (cadr p))

(define (res-proof-pos p)
  (caddr p))

(define (res-proof-neg p)
  (cadddr p))

;; sprawdza strukturę, ale nie poprawność dowodu
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; procedura sprawdzająca poprawność dowodu
(define (check-proof pf form)
  (define (run-axiom c)
    (displayln (list 'checking 'axiom c))
    (and (member c (cnf-clauses form))
         (clause-lits c)))
  (define (run-res x cpos cneg)
    (displayln (list 'checking 'resolution 'of x 'for cpos 'and cneg))
    (and (findf (lambda (l) (and (literal-pol l)
                                 (eq? x (literal-var l))))
                cpos)
         (findf (lambda (l) (and (not (literal-pol l))
                                 (eq? x (literal-var l))))
                cneg)
         (append (remove* (list (literal true x))  cpos)
                 (remove* (list (literal false x)) cneg))))
  (define (run-proof pf)
    (cond
     [(axiom? pf) (run-axiom (axiom-clause pf))]
     [(res? pf)   (run-res (res-var pf)
                           (run-proof (res-proof-pos pf))
                           (run-proof (res-proof-neg pf)))]
     [else        false]))
  (null? (run-proof pf)))


;; reprezentacja wewnętrzna

;; sprawdza posortowanie w porządku ściśle rosnącym, bez duplikatów
(define (sorted? vs)
  (or (null? vs)
      (null? (cdr vs))
      (and (var<? (car vs) (cadr vs))
           (sorted? (cdr vs)))))

(define (sorted-varlist? x)
  (and (list? x)
       (andmap (var? x))
       (sorted? x)))

;; klauzulę reprezentujemy jako parę list — osobno wystąpienia pozytywne i negatywne. Dodatkowo
;; pamiętamy wyprowadzenie tej klauzuli (dowód) i jej rozmiar.
(define (res-clause? x)
  (and (tagged-tuple? 'res-int 5 x)
       (sorted-varlist? (second x))
       (sorted-varlist? (third x))
       (= (fourth x) (+ (length (second x)) (length (third x))))
       (proof? (fifth x))))

(define (res-clause pos neg proof)
  (list 'res-int pos neg (+ (length pos) (length neg)) proof))

(define (res-clause-pos c)
  (second c))

(define (res-clause-neg c)
  (third c))

(define (res-clause-size c)
  (fourth c))

(define (res-clause-proof c)
  (fifth c))

;; przedstawia klauzulę jako parę list zmiennych występujących odpowiednio pozytywnie i negatywnie
(define (print-res-clause c)
  (list (res-clause-pos c) (res-clause-neg c)))

;; sprawdzanie klauzuli sprzecznej
(define (clause-false? c)
  (and (null? (res-clause-pos c))
       (null? (res-clause-neg c))))

;; pomocnicze procedury: scalanie i usuwanie duplikatów z list posortowanych
(define (merge-vars xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [(var<? (car xs) (car ys))
         (cons (car xs) (merge-vars (cdr xs) ys))]
        [(var<? (car ys) (car xs))
         (cons (car ys) (merge-vars xs (cdr ys)))]
        [else (cons (car xs) (merge-vars (cdr xs) (cdr ys)))]))

(define (remove-duplicates-vars xs)
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [(var=? (car xs) (cadr xs)) (remove-duplicates-vars (cdr xs))]
        [else (cons (car xs) (remove-duplicates-vars (cdr xs)))]))

(define (rev-append xs ys)
  (if (null? xs) ys
      (rev-append (cdr xs) (cons (car xs) ys))))

(define (clause-trivial? c)
    (define (is-it? pos neg)
      (cond [(null? pos) #f]
            [(not (not (member (car pos) neg))) #t]
            [else (if (null? (cdr pos))
                      #f
                      (is-it? (cdr pos) neg))]))
  (is-it? (res-clause-pos c) (res-clause-neg c)))



(define (resolve c1 c2)
  (define (negation? l neg)
  (cond [(null? neg) #f]
        [else (if (not (not (member l neg)))
                  #t
                  #f)]))
  (define (negations pos neg)
    (cond [(null? pos) '()]
          [(null? neg) '()]
          [else (if (negation? (car pos) neg)
                    (car pos)
                    (negations (cdr pos) neg))]))
  (define (remove-it ls var)
    (cond [(null? ls) '()]
          [else (if (eq? (car ls) var)
            (cdr ls)
            (cons (car ls) (remove-it (cdr ls) var)))]))
  (let ([one-way (negations (res-clause-pos c1) (res-clause-neg c2))]
        [backword (negations (res-clause-pos c2) (res-clause-neg c1))])
    (if (null? backword)
        (if (null? one-way)
            #f
            (let* ([pos-new (remove-it (res-clause-pos c1) one-way)]
                   [neg-new (remove-it (res-clause-neg c2) one-way)])
              (res-clause pos-new neg-new (proof-res one-way (res-clause-proof c1) (res-clause-proof c2)))))
        (let* ([pos-new (remove-it (res-clause-pos c2) backword)]
               [neg-new (remove-it (res-clause-neg c1) backword)])
          (res-clause pos-new neg-new (proof-res backword (res-clause-proof c2) (res-clause-proof c1)))))))



(define (resolve-single-prove s-clause checked pending)
  (define (compare s-clause list) (cond
                                    [(null? list) #f]
                                    [(eq? #f (resolve s-clause (car list))) (compare s-clause (cdr list))]
                                    [(eq? #t (not (not (resolve s-clause (car list))))) #t]))
  (define (replace s-clause lis acc) (cond
                                      [(null? lis) acc]  
                                      [(eq? #t (compare s-clause lis)) (replace s-clause (cdr lis) (append acc (list (resolve s-clause (car lis)))))]
                                      [(eq? #f (compare s-clause lis)) (replace s-clause (cdr lis) (append acc (list (car lis))))]))
  (let* ((new-pending (replace s-clause pending '()))
         (new-checked (replace s-clause checked '()))
    (resolvents   (filter-map (lambda (c) (resolve c s-clause))
                                     checked))
         (sorted-rs    (sort resolvents < #:key res-clause-size)))
    (subsume-add-prove (cons s-clause new-checked) new-pending sorted-rs)))

;; wstawianie klauzuli w posortowaną względem rozmiaru listę klauzul
(define (insert nc ncs)
  (cond
   [(null? ncs)                     (list nc)]
   [(< (res-clause-size nc)
       (res-clause-size (car ncs))) (cons nc ncs)]
   [else                            (cons (car ncs) (insert nc (cdr ncs)))]))

;; sortowanie klauzul względem rozmiaru (funkcja biblioteczna sort)
(define (sort-clauses cs)
  (sort cs < #:key res-clause-size))

;; główna procedura szukająca dowodu sprzeczności
;; zakładamy że w checked i pending nigdy nie ma klauzuli sprzecznej
(define (resolve-prove checked pending)
  (cond
   ;; jeśli lista pending jest pusta, to checked jest zamknięta na rezolucję czyli spełnialna
   [(null? pending) (generate-valuation (sort-clauses checked))]
   ;; jeśli klauzula ma jeden literał, to możemy traktować łatwo i efektywnie ją przetworzyć
   [(= 1 (res-clause-size (car pending)))
    (resolve-single-prove (car pending) checked (cdr pending))]
   ;; w przeciwnym wypadku wykonujemy rezolucję z wszystkimi klauzulami już sprawdzonymi, a
   ;; następnie dodajemy otrzymane klauzule do zbioru i kontynuujemy obliczenia
   [else
    (let* ((next-clause  (car pending))
           (rest-pending (cdr pending))
           (resolvents   (filter-map (lambda (c) (resolve c next-clause))
                                     checked))
           (sorted-rs    (sort-clauses resolvents)))
      (subsume-add-prove (cons next-clause checked) rest-pending sorted-rs))]))



;; procedura upraszczająca stan obliczeń biorąc pod uwagę świeżo wygenerowane klauzule i
;; kontynuująca obliczenia. Do uzupełnienia.
(define (subsume-add-prove checked pending new)
   (define (sublist l1 l2) (cond
                              [(null? l1) #t]
                              [else (if
                                     (not (not (member (car l1) l2)))
                                     (sublist (cdr l1) l2)
                                     #f)]))
    (define (easier-list c list) (cond
                                   [(null? list) #f]
                                   [else (cond [(easier c (car list)) #t]
                                         [else (easier-list c (cdr list))])]));łatwiejsze od wszystkich
    (define (easier c1 c2) (if (and (sublist (res-clause-pos c1) (res-clause-pos c2)) (sublist (res-clause-neg c1) (res-clause-neg c2)))
                               #t
                               #f)) ; czy poz c1 są podlistą c2 i czy neg z c1 są podlistą c2
    (define (remove el xs acc) (cond
                                 [(null? xs ) el]
                                 [(easier (car xs) el) (remove el (cdr xs) acc)]
                                 [else  (cons acc (car xs))]));-usowa el łatwiejsze -poslisy czy łątwiejsza -musi być służsza i mies te same el + jakiś jeszcze
    ;; TODO: zaimplementuj!
    ;; Poniższa implementacja nie sprawdza czy nowa klauzula nie jest lepsza (bądź gorsza) od już
    ;; rozpatrzonych; popraw to!
  (cond
   [(null? new)                 (resolve-prove checked pending)]
   ;; jeśli klauzula do przetworzenia jest sprzeczna to jej wyprowadzenie jest dowodem sprzeczności
   ;; początkowej formuły
   [(clause-false? (car new))   (list 'unsat (res-clause-proof (car new)))]
   ;; jeśli klauzula jest trywialna to nie ma potrzeby jej przetwarzać
   [(clause-trivial? (car new)) (subsume-add-prove checked pending (cdr new))]
   [else
    (if (or (easier-list (car new) checked) (easier-list (car new) pending))
        (subsume-add-prove checked pending (cdr new))
        (subsume-add-prove (remove new checked '()) (insert (car new) (remove new pending '())) (cdr new)))
    ;; Poniższa implementacja nie sprawdza czy nowa klauzula nie jest lepsza (bądź gorsza) od już
    ;; rozpatrzonych; popraw to!
    ]))


(define (generate-valuation resolved)
  (define (remove-claus-pos val list acc)
    (cond [(null? list) acc]
          [else (if (is-in-pos? val (car list))
                    (remove-claus-pos val (remove* (car (cdr list)) list) (remove* (car (cdr list)) list))
                    (remove-claus-pos val (cdr list) list))]))
    (define (remove-claus-neg val list acc)
      (cond [(null? list) acc]
            [else (if (is-in-neg? val (car list))
                      (remove-claus-neg val (remove* (car (cdr list)) list) (remove* (car (cdr list)) list))
                      (remove-claus-neg val (cdr list) list))]))
    (define (remove-from-list-pos val list acc)
    (if (null? list)
        acc
        (remove-from-list-pos  val (cdr list) (cons acc (remove* (list val) (res-clause-pos (car list)))))))
  (define (remove-from-list-neg val list acc)
    (if (null? list)
        acc
        (remove-from-list-neg  val (cdr list) (cons acc (remove* (list val) (res-clause-neg (car list)))))))

    (define (is-in-pos? val c)
      (define (in-pos? val list)
        (if (null? list)
            #f
            (if (eq? val (car list))
                #t
                (in-pos? var (cdr list)))))
      (in-pos? val (res-clause-pos c)))
  (define (is-in-neg? val c)
    (define (in-neg? val list)
      (if (null? list)
          #f
          (if (eq? val (car list))
              #t
              (in-neg? var (cdr list)))))
    (in-neg? val (res-clause-neg c)))
  (define (helper resolver acc)
    (cond [(null? resolved) (list 'sat acc)]
          [(= 1 (length resolved)) (if (= 1 (length (res-clause-pos (car resolved))))
                                              (list 'sat (cons (res-clause-pos (car resolved)) '(#t)))
                                              (list 'sat (cons (res-clause-neg (car resolved)) '(#f))))]
          
          [else (cond [(= 1 (res-clause-size (car resolved)))
                         (if (= 1 (length (res-clause-pos (car resolved))))
                             (let* ((l (car (res-clause-pos (car resolved))))
                                    (val (cons acc (list l '(#t))));;warościowanie już z nową zmienną
                                    (new (remove-from-list-pos l (remove-claus-pos l resolved '()) '())))
                                             (helper new val))
                             (let* ((l (car (res-clause-neg (car resolved))))
                                    (val (cons acc (list l '(#f))));;warościowanie już z nową zmienną
                                    (new (remove-from-list-neg l (remove-claus-neg l resolved '()) '())))
                                             (helper new val)))]
                      [(not (null? (res-clause-pos (car resolved))));;pierwsza klauzula ma pozytywne
                         (let* ((l (car (res-clause-pos (car resolved))));;biorę pierwszą zmienną i ustalam na #t
                                (val (cons acc (list l '(#t))))
                                (new (remove-from-list-pos l (remove-claus-pos l resolved '()) '())))
                                (helper new val))]              
                        [(not (null? (res-clause-neg (car resolved))))
                         (let* ((l (car (res-clause-neg (car resolved))))
                                (val (cons acc (list l '(#f))))
                                (new (remove-from-list-neg l (remove-claus-neg l resolved '()) '())))
                                (helper new val))]
                        [else (helper (cdr resolved) acc)])])) 
  (helper resolved '()))
                    


;; procedura przetwarzające wejściowy CNF na wewnętrzną reprezentację klauzul
(define (form->clauses f)
  (define (conv-clause c)
    (define (aux ls pos neg)
      (cond
       [(null? ls)
        (res-clause (remove-duplicates-vars (sort pos var<?))
                    (remove-duplicates-vars (sort neg var<?))
                    (proof-axiom c))]
       [(literal-pol (car ls))
        (aux (cdr ls)
             (cons (literal-var (car ls)) pos)
             neg)]
       [else
        (aux (cdr ls)
             pos
             (cons (literal-var (car ls)) neg))]))
    (aux (clause-lits c) null null))
  (map conv-clause (cnf-clauses f)))

(define (prove form)
  (let* ((clauses (form->clauses form)))
    (subsume-add-prove '() '() clauses)))

;; procedura testująca: próbuje dowieść sprzeczność formuły i sprawdza czy wygenerowany
;; dowód/waluacja są poprawne. Uwaga: żeby działała dla formuł spełnialnych trzeba umieć wygenerować
;; poprawną waluację.
(define (prove-and-check form)
  (let* ((res (prove form))
         (sat (car res))
         (pf-val (cadr res)))
    (if (eq? sat 'sat)
        (valuate-partial pf-val form)
        (check-proof pf-val form))))

#|
--------------------------------
część -------TESTY
--------------------------------
|#

;;; TODO: poniżej wpisz swoje testy


(prove-and-check '(cnf (clause (literal #t a)) (clause (literal #f a))))
(prove-and-check '(cnf (clause (literal #t a) (literal #f a))))
(prove-and-check '(cnf (clause (literal #f p) (literal #t q) (literal #t p) (literal #t q))))


(define p (literal #t 'p))
(define q (literal #t 'q))
(define r (literal #t 'r))
(define s (literal #t 's))
(define ~p (literal #f 'p))
(define ~q (literal #f 'q))
(define ~r (literal #f 'r))
(define ~s (literal #f 's))
(define k (literal #t 'k))
(define ~k (literal #f 'k))
(define c (cnf (clause ~p q) (clause ~p ~r s) (clause ~q r) (clause p) (clause ~s)))


(prove-and-check c)
(prove c)